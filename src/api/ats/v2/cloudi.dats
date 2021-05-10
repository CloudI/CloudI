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

void c_set_c_state(void * c_api, void * state)
{
    ((cloudi_instance_t *)c_api)->state = state;
}
/*
#define cloudi_get_response(api)               ((api)->response)
#define cloudi_get_response_size(api)          ((api)->response_size)
#define cloudi_get_response_info(api)          ((api)->response_info)
#define cloudi_get_response_info_size(api)     ((api)->response_info_size)
#define cloudi_get_trans_id_count(api)         ((api)->trans_id_count)
#define cloudi_get_trans_id(api, i)            (&((api)->trans_id[i * 16]))
*/
uint32_t c_get_subscribe_count(void * c_api)
{
    return ((cloudi_instance_t *)c_api)->subscribe_count;
}
/*
#define cloudi_get_process_index(api)          ((api)->process_index)
#define cloudi_get_process_count(api)          ((api)->process_count)
#define cloudi_get_process_count_max(api)      ((api)->process_count_max)
#define cloudi_get_process_count_min(api)      ((api)->process_count_min)
#define cloudi_get_prefix(api)                 ((api)->prefix)
#define cloudi_get_timeout_initialize(api)     ((api)->timeout_initialize)
#define cloudi_get_timeout_async(api)          ((api)->timeout_async)
#define cloudi_get_timeout_sync(api)           ((api)->timeout_sync)
#define cloudi_get_timeout_terminate(api)      ((api)->timeout_terminate)
#define cloudi_get_priority_default(api)       ((api)->priority_default)
*/
%}
macdef c_int_success = 0
macdef c_int_terminate = $extval(int, "cloudi_terminate")
macdef c_int_timeout = $extval(int, "cloudi_timeout")

extern castfn sz_u32(x: size_t): uint32
extern castfn ssz_u32(x: ssize_t): uint32
extern castfn u_u32(x: uint): uint32
extern castfn u32_u(x: uint32): uint
extern castfn i_u32(x: int): uint32
extern castfn i_i8(x: int): int8

(* functions called in templates are required to be external (global)
 * (https://github.com/githwxi/ATS-Postiats/issues/180#issuecomment-363285569)
 * (Otherwise PMVerr appears in the C ATS output)
 *)
extern fn {s:vt@ype}
state_p_new
    (state_value: s): Ptr1
extern fn {s:vt@ype}
state_p_destroy {l:agz}
    (state_p: ptr(l)): s
extern fn
callback_name
    (c_api: ptr,
     r: $CLOUDI.response_ptr): ptr
extern fn
callback_request_info
    (c_api: ptr,
     r: $CLOUDI.response_ptr,
     size: &uint32): ptr
extern fn
callback_request
    (c_api: ptr,
     r: $CLOUDI.response_ptr,
     size: &uint32): ptr
extern fn
callback_response_info
    (c_api: ptr,
     r: $CLOUDI.response_ptr,
     size: &uint32): ptr
extern fn
callback_response
    (c_api: ptr,
     r: $CLOUDI.response_ptr,
     size: &uint32): ptr

(* C CloudI API functions
 *)
extern fn
c_set_c_state:
    (ptr,
     ptr) -> void = "ext#"
extern fn
c_get_subscribe_count:
    (ptr) -> uint32 = "ext#"
extern fn
c_initialize: {l1:agz}{l2:addr}
    (!$CLOUDI.c_instance? @ l1 >> $CLOUDI.c_instance @ l1 |
     ptr(l1), uint, ptr(l2)) -> intGte(0) = "ext#cloudi_initialize"
extern fn
c_destroy: {l1:agz}{l2:addr}
    (!$CLOUDI.c_instance @ l1 |
     ptr(l1)) -> ptr(l2) = "ext#cloudi_destroy"
extern fn
c_initialize_thread_count: {l1:agz}
    (!uintGt(0) @ l1 |
     ptr(l1)) -> intGte(0) = "ext#cloudi_initialize_thread_count"
extern fn
c_subscribe: {l1:agz}
    (ptr,
     ptr(l1),
     $CLOUDI.c_callback) -> intGte(0) = "ext#cloudi_subscribe"
extern fn
c_subscribe_count: {l1:agz}
    (ptr,
     ptr(l1)) -> intGte(0) = "ext#cloudi_subscribe_count"
extern fn
c_unsubscribe: {l1:agz}
    (ptr,
     ptr(l1)) -> intGte(0) = "ext#cloudi_unsubscribe"
extern fn
c_send_async: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint) -> intGte(0) = "ext#cloudi_send_async"
extern fn
c_send_async_: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint,
     ptr,
     uint,
     uint,
     int) -> intGte(0) = "ext#cloudi_send_async_"
extern fn
c_send_sync: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint) -> intGte(0) = "ext#cloudi_send_sync"
extern fn
c_send_sync_: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint,
     ptr,
     uint,
     uint,
     int) -> intGte(0) = "ext#cloudi_send_sync_"
extern fn
c_mcast_async: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint) -> intGte(0) = "ext#cloudi_mcast_async"
extern fn
c_mcast_async_: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1),
     ptr,
     uint,
     ptr,
     uint,
     uint,
     int) -> intGte(0) = "ext#cloudi_mcast_async_"
extern fn
c_forward:
    (ptr,
     int,
     ptr,
     ptr,
     uint32,
     ptr,
     uint32,
     uint32,
     int8,
     ptr,
     ptr,
     uint32) -> intGte(0) = "ext#cloudi_forward"
extern fn
c_return:
    (ptr,
     int,
     ptr,
     ptr,
     ptr,
     uint32,
     ptr,
     uint32,
     uint32,
     ptr,
     ptr,
     uint32) -> intGte(0) = "ext#cloudi_return"
extern fn
c_recv_async: {l1:agz}
    (!char @ l1 |
     ptr,
     uint,
     ptr(l1),
     int) -> intGte(0) = "ext#cloudi_recv_async"
extern fn
c_poll:
    (ptr,
     int) -> intGte(0) = "ext#cloudi_poll"
extern fn
c_shutdown: {l1:agz}
    (!char @ l1 |
     ptr,
     ptr(l1)) -> intGte(0) = "ext#cloudi_shutdown"
extern fn
c_free_name:
    (ptr) -> void = "ext#cloudi_free_name"
extern fn
c_free_pattern:
    (ptr) -> void = "ext#cloudi_free_pattern"
extern fn
c_free_request_info:
    (ptr) -> void = "ext#cloudi_free_request_info"
extern fn
c_free_request:
    (ptr) -> void = "ext#cloudi_free_request"
extern fn
c_free_response_info:
    (ptr) -> void = "ext#cloudi_free_response_info"
extern fn
c_free_response:
    (ptr) -> void = "ext#cloudi_free_response"

(* ATS CloudI API type
 *)
datavtype
instance_(s:vt@ype) = {l1,l2:agz} INSTANCE of (@{
  c_ptr = ($CLOUDI.c_instance @ l1, mfree_gc_v(l1) | ptr(l1))
, state_p = ptr(l2)
, terminate_return_value = bool
})
assume $CLOUDI.instance_vtype(s:vt@ype) = instance_(s)

(* support for threads usage with the ATS CloudI API
 *)
datavtype
threads_ = {l0,l1:agz} THREADS of (@{
  thread_count = uintGt(0)
, running = intGte(0)
, mutex = $ATHREAD.mutex_vt(l0)
, condvar = $ATHREAD.condvar_vt(l1)
})
assume $CLOUDI.threads_vtype = threads_

fn
instance_c_ptr {s:vt@ype}
    (api: !$CLOUDI.instance(s)): ptr = let
    val INSTANCE(@{c_ptr = (_, _ | c_api), ...}) = api
in
    c_api
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

fn {a:t@ype}
result_value {s:vt@ype}
    (value: a,
     status: intGte(0),
     api: !$CLOUDI.instance(s)): $CLOUDI.Result(a) =
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
$CLOUDI.Strptr
    (str) = let
    val size: uint32 = ssz_u32(strptr_length(str))
    val p: ptr = strptr2ptr(str)
    prval () = $UNSAFE.cast2void(str)
in
    $CLOUDI.Ptr(p, size)
end

implement
callback_name
    (c_api,
     r) =
case+ r of
  | ~$CLOUDI.String(str) =>
    string2ptr(str)
  | ~$CLOUDI.Ptr(p, _) => let
    val () = c_free_name(c_api)
in
    p
end

(* for subscribe only
fn
callback_pattern
    (c_api: ptr,
     r: $CLOUDI.response_ptr): ptr =
case+ r of
  | ~$CLOUDI.String(str) =>
    string2ptr(str)
  | ~$CLOUDI.Ptr(p, _) => let
    val () = c_free_pattern(c_api)
in
    p
end
*)

implement
callback_request_info
    (c_api,
     r,
     size) =
case+ r of
  | ~$CLOUDI.String(str) => let
    val () = size := sz_u32(string0_length(str))
in
    string2ptr(str)
end
  | ~$CLOUDI.Ptr(p, size_p) => let
    val () = c_free_request_info(c_api)
    val () = size := size_p
in
    p
end

implement
callback_request
    (c_api,
     r,
     size) =
case+ r of
  | ~$CLOUDI.String(str) => let
    val () = size := sz_u32(string0_length(str))
in
    string2ptr(str)
end
  | ~$CLOUDI.Ptr(p, size_p) => let
    val () = c_free_request(c_api)
    val () = size := size_p
in
    p
end

implement
callback_response_info
    (c_api,
     r,
     size) =
case+ r of
  | ~$CLOUDI.String(str) => let
    val () = size := sz_u32(string0_length(str))
in
    string2ptr(str)
end
  | ~$CLOUDI.Ptr(p, size_p) => let
    val () = c_free_response_info(c_api)
    val () = size := size_p
in
    p
end

implement
callback_response
    (c_api,
     r,
     size) =
case+ r of
  | ~$CLOUDI.String(str) => let
    val () = size := sz_u32(string0_length(str))
in
    string2ptr(str)
end
  | ~$CLOUDI.Ptr(p, size_p) => let
    val () = c_free_response(c_api)
    val () = size := size_p
in
    p
end

implement {s}
$CLOUDI.callback_attach
    (callback,
     request_type,
     name,
     pattern,
     request_info,
     request_info_size,
     request,
     request_size,
     timeout,
     priority,
     trans_id,
     pid,
     pid_size,
     c_state,
     c_api) = let
    val name_ats: string = $UNSAFE.castvwtp0{string}(name)
    val pattern_ats: string = $UNSAFE.castvwtp0{string}(pattern)
    val request_info_size_ats = g0uint2uint_uint32_uint(request_info_size)
    val request_size_ats = g0uint2uint_uint32_uint(request_size)
    val timeout_ats = g0uint2uint_uint32_uint(timeout)
    val priority_ats = g0int2int_int8_int(priority)
    val pid_size_ats = g0uint2uint_uint32_uint(pid_size)
    val api = $UNSAFE.castvwtp1{$CLOUDI.instance(s)}(c_state)
    val INSTANCE(@{state_p = state_p, ...}) = api
    val state = $UNSAFE.castvwtp1{$CLOUDI.stateptr(s)}(state_p)
    val response_ats = try callback(request_type, name_ats, pattern_ats,
                                    request_info, request_info_size_ats,
                                    request, request_size_ats,
                                    timeout_ats, priority_ats, trans_id,
                                    pid, pid_size_ats,
                                    state, api) with
      | ~$CLOUDI.Terminate() =>
        $CLOUDI.Null()
      | ~$CLOUDI.FatalError() => let
        val () = exit_errmsg_void(1, $mylocation)
    in
        $CLOUDI.Null()
    end
      | e:exn => let
        val () = fprintln!(stderr_ref, $mylocation)
        prval () = $UNSAFE.cast2void(e)
    in
        $CLOUDI.Null()
    end
    prval () = $UNSAFE.cast2void(api)
    prval () = $UNSAFE.cast2void(state)
in
    case+ response_ats of
      | ~$CLOUDI.Response(response) => let
        var response_info_size: uint32 = i_u32(0)
        val response_info_ptr = callback_response_info(c_api,
                                                       $CLOUDI.String(""),
                                                       response_info_size)
        var response_size: uint32 = i_u32(0)
        val response_ptr = callback_response(c_api,
                                             response,
                                             response_size)
        val status = c_return(c_api, request_type, name, pattern,
                              response_info_ptr, response_info_size,
                              response_ptr, response_size,
                              timeout, trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.ResponseInfo(response_info, response) => let
        var response_info_size: uint32 = i_u32(0)
        val response_info_ptr = callback_response_info(c_api,
                                                       response_info,
                                                       response_info_size)
        var response_size: uint32 = i_u32(0)
        val response_ptr = callback_response(c_api,
                                             response,
                                             response_size)
        val status = c_return(c_api, request_type, name, pattern,
                              response_info_ptr, response_info_size,
                              response_ptr, response_size,
                              timeout, trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Forward(name_new, request_info_new, request_new) => let
        val name_new_ptr = callback_name(c_api, name_new)
        var request_info_size_new: uint32 = i_u32(0)
        val request_info_new_ptr = callback_request_info(c_api,
                                                         request_info_new,
                                                         request_info_size_new)
        var request_size_new: uint32 = i_u32(0)
        val request_new_ptr = callback_request(c_api,
                                               request_new,
                                               request_size_new)
        val status = c_forward(c_api, request_type, name_new_ptr,
                               request_info_new_ptr, request_info_size_new,
                               request_new_ptr, request_size_new,
                               timeout, priority,
                               trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Forward_(name_new, request_info_new, request_new,
                          timeout_new, priority_new) => let
        val name_new_ptr = callback_name(c_api, name_new)
        var request_info_size_new: uint32 = i_u32(0)
        val request_info_new_ptr = callback_request_info(c_api,
                                                         request_info_new,
                                                         request_info_size_new)
        var request_size_new: uint32 = i_u32(0)
        val request_new_ptr = callback_request(c_api,
                                               request_new,
                                               request_size_new)
        val status = c_forward(c_api, request_type, name_new_ptr,
                               request_info_new_ptr, request_info_size_new,
                               request_new_ptr, request_size_new,
                               u_u32(timeout_new), i_i8(priority_new),
                               trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Null() => let
        var response_info_size: uint32 = i_u32(0)
        val response_info_ptr = callback_response_info(c_api,
                                                       $CLOUDI.String(""),
                                                       response_info_size)
        var response_size: uint32 = i_u32(0)
        val response_ptr = callback_response(c_api,
                                             $CLOUDI.String(""),
                                             response_size)
        val status = c_return(c_api, request_type, name, pattern,
                              response_info_ptr, response_info_size,
                              response_ptr, response_size,
                              timeout, trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.NullError(error) => let
        val () = fprintln!(stderr_ref, error)
        var response_info_size: uint32 = i_u32(0)
        val response_info_ptr = callback_response_info(c_api,
                                                       $CLOUDI.String(""),
                                                       response_info_size)
        var response_size: uint32 = i_u32(0)
        val response_ptr = callback_response(c_api,
                                             $CLOUDI.String(""),
                                             response_size)
        val status = c_return(c_api, request_type, name, pattern,
                              response_info_ptr, response_info_size,
                              response_ptr, response_size,
                              timeout, trans_id, pid, pid_size)
    in
        assertloc(status = 0)
    end
end

implement
$CLOUDI.thread_count() = let
    var count: uintGt(0) with count_pfgc = i2u(1)
    val status = c_initialize_thread_count(count_pfgc | addr@count)
    val () = assertloc(status = 0)
in
    count
end

implement {s}
$CLOUDI.new(thread_index, state_value, terminate_return_value) = let
    val (c_pfgc, c_pfat | c_api) = ptr_alloc<$CLOUDI.c_instance>()
    val status = c_initialize(c_pfgc | c_api, thread_index, the_null_ptr)
in
    if (status = c_int_success) then let
        val state_p: Ptr1 = state_p_new<s>(state_value)
        val api = INSTANCE(@{
            c_ptr = (c_pfgc, c_pfat | c_api),
            state_p = state_p,
            terminate_return_value = terminate_return_value})
        val () = c_set_c_state(c_api, $UNSAFE.castvwtp1{ptr}(api))
    in
        $CLOUDI.Ok(api)
    end
    else let
        val () = ptr_free(c_pfat, c_pfgc | c_api)
        prval () = $UNSAFE.cast2void(state_value)
    in
        $CLOUDI.Error(status)
    end
end

implement {s}
$CLOUDI.destroy(api) = let
    val ~INSTANCE(@{
        c_ptr = (c_pfgc, c_pfat | c_api),
        state_p = state_p, ...}) = api
    val _ = c_destroy(c_pfgc | c_api)
    val () = ptr_free(c_pfat, c_pfgc | c_api)
in
    state_p_destroy<s>(state_p)
end

implement {s}
$CLOUDI.destroy2void(api) = let
    val state_value: s = $CLOUDI.destroy(api)
    prval () = $UNSAFE.cast2void(state_value)
in
    ()
end

implement
$CLOUDI.subscribe(api, suffix, f) = let
    val c_api = instance_c_ptr(api)
in
    result_value<unit>(unit(), c_subscribe(c_api, string2ptr(suffix), f), api)
end

implement
$CLOUDI.subscribe_count(api, suffix) = let
    val c_api = instance_c_ptr(api)
    val status = c_subscribe_count(c_api, string2ptr(suffix))
    val value = u32_u(c_get_subscribe_count(c_api))
in
    result_value<uint>(value, status, api)
end

implement
$CLOUDI.unsubscribe(api, suffix) = let
    val c_api = instance_c_ptr(api)
in
    result_value<unit>(unit(), c_unsubscribe(c_api, string2ptr(suffix)), api)
end

implement
$CLOUDI.poll(api, timeout) = let
    val c_api = instance_c_ptr(api)
    val status = c_poll(c_api, timeout)
in
    if (status = c_int_timeout) then
        $CLOUDI.Ok(true)
    else if (status = c_int_success) then
        $CLOUDI.Ok(false)
    else
        $CLOUDI.Error(status)
end

fn
threads_new
    (thread_count: uintGt(0)): $CLOUDI.threads = let
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
     f: (uint) -> void): void = let
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
     f: (uint) -> void): void = let
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
    (threads: !$CLOUDI.threads): void = let
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
    (threads: $CLOUDI.threads): void = let
    val ~THREADS(@{mutex = mutex, condvar = condvar, ...}) = threads
    val () = $ATHREAD.mutex_vt_destroy(mutex)
    val () = $ATHREAD.condvar_vt_destroy(condvar)
in
    ()
end

implement
$CLOUDI.threads_create(thread_count, f) = let
    val threads = threads_new(thread_count)
    val () = threads_add(threads, f)
in
    threads
end

implement
$CLOUDI.threads_wait(threads) = let
    val () = threads_remove(threads)
in
    threads_destroy(threads)
end
