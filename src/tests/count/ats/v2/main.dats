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

vtypedef state_type = int

fn
request_ats
    (request_type: $CLOUDI.request_type,
     name: string,
     pattern: string,
     request_info: Ptr1,
     request_info_size: uint,
     request: Ptr1,
     request_size: uint,
     timeout: uint,
     priority: int,
     trans_id: !$CLOUDI.trans_id_ptr,
     pid: Ptr1,
     pid_size: uint,
     state: !$CLOUDI.stateptr(state_type),
     api: !$CLOUDI.instance(state_type)): $CLOUDI.response = let
    val state_value: int = aptr_get_elt<int>(state) + 1
    val () = println!("count == ", state_value, " ats2")
    val () = aptr_set_elt(state, state_value)
in
    $CLOUDI.Response($CLOUDI.Strptr(g0int2string_int(state_value)))
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
     api_c: ptr): void = 
    $CLOUDI.callback_attach(request_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
task
    (thread_index: uint): void = let
    var state_value: int = 0
in
    case+ $CLOUDI.new(thread_index, state_value, true) of
      | ~$CLOUDI.Ok(api) => let
        val () = case+ $CLOUDI.subscribe(api, "ats2/get", request) of
          | ~$CLOUDI.Ok(_) =>
            ()
          | ~$CLOUDI.Error(status) =>
            fprintln!(stderr_ref, "error ", status)
        val () = case+ $CLOUDI.poll(api, ~1) of
          | ~$CLOUDI.Ok(_) =>
            println!("terminate count ats2")
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

