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
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "cloudi.hats"
staload UNSAFE = "prelude/SATS/unsafe.sats"

vtypedef state_type = unit

#define DESTINATION "/tests/msg_size/erlang"

fn
request_ats
    (request_type: $CLOUDI.request_type,
     name: string,
     pattern: string,
     request_info: $CLOUDI.memory_ptr,
     request: $CLOUDI.memory_ptr,
     timeout: $CLOUDI.timeout,
     priority: $CLOUDI.priority,
     trans_id: !$CLOUDI.trans_id_ptr,
     source: !$CLOUDI.memory_ptr,
     state: !$CLOUDI.stateptr(state_type),
     api: !$CLOUDI.instance(state_type)):
    $CLOUDI.response = let
    val request_info_next = $CLOUDI.memory2free(request_info)
    val $CLOUDI.Ptr(p, size) = request
    var i: uint = $UNSAFE.ptr1_get<uint>(p)
    val () = if (i = i2u(1073741823)) then
        i := i2u(0)
    else
        i := i + i2u(1)
    val () = println!("forward #", i, " ats2 to ", DESTINATION,
                      " (with timeout ", timeout, " ms)")
    val () = $UNSAFE.ptr1_set<uint>(p, i)
    val request_next = $CLOUDI.memory2free(request)
in
    $CLOUDI.Forward($CLOUDI.StringLiteral(DESTINATION),
                    request_info_next, request_next)
end

fn
request
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
     api_c: ptr):
    void =
    $CLOUDI.callback_attach(request_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
task
    (thread_index: uint):
    void = let
    var state_value: unit = unit()
in
    case+ $CLOUDI.new(thread_index, state_value, true) of
      | ~$CLOUDI.Ok(api) => let
        val () = case+ $CLOUDI.subscribe(api, "ats2", request) of
          | ~$CLOUDI.Ok(_) =>
            ()
          | ~$CLOUDI.Error(status) =>
            assertloc(status = 0)
        val () = case+ $CLOUDI.poll(api, ~1) of
          | ~$CLOUDI.Ok(_) =>
            println!("terminate msg_size ats2")
          | ~$CLOUDI.Error(status) =>
            fprintln!(stderr_ref, "error ", status)
        val () = $CLOUDI.destroy2void(api)
    in
        ()
    end
      | ~$CLOUDI.Error(status) =>
        fprintln!(stderr_ref, "error ", status)
end

implement main0 () = let
    val thread_count = $CLOUDI.thread_count()
    val threads = $CLOUDI.threads_create(thread_count, task)
in
    $CLOUDI.threads_wait(threads)
end

