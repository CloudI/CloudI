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
#include "{$PATSPRE}/lmacrodef.hats"
#include "cloudi.hats"

typedef state_type = int

fn
request_ats
    (request_type: $CLOUDI.request_type,
     name: string,
     pattern: string,
     request_info: ptr,
     request_info_size: uint,
     request: ptr,
     request_size: uint,
     timeout: uint,
     priority: int,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint,
     state: state_type,
     api: !$CLOUDI.instance(state_type)): $CLOUDI.response = let
    (* XXX switch to using state_pfgc/state_p via cptr(a,p) or some other way
    val () = !state :=+ 1
    *)
    val () = println!("count == ", state, " ats2")
in
    $CLOUDI.Null()
end

fn
request_
    (request_type: $CLOUDI.request_type,
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
     c_api: ptr): void = 
    $CLOUDI.callback_attach(request_ats,
                            request_type, name, pattern,
                            request_info, request_info_size,
                            request, request_size,
                            timeout, priority, trans_id,
                            pid, pid_size, state, c_api)

fn
task
    (thread_index: uint): void = let
    var state: int = 0
in
    case+ $CLOUDI.new(view@state | thread_index, addr@state, true) of
      | ~$CLOUDI.Ok(api) => let
        val () = case+ $CLOUDI.subscribe(api, "ats2/get", request_) of
          | ~$CLOUDI.Ok(_) =>
            ()
          | ~$CLOUDI.Error(status) =>
            fprintln!(stderr_ref, "error ", status)
        val () = case+ $CLOUDI.poll(api, ~1) of
          | ~$CLOUDI.Ok(_) =>
            println!("terminate count ats2")
          | ~$CLOUDI.Error(status) =>
            fprintln!(stderr_ref, "error ", status)
        in
            $CLOUDI.destroy(api)
        end
      | ~$CLOUDI.Error(status) =>
        fprintln!(stderr_ref, "error ", status)
end

implement main0 () = let
    val thread_count = $CLOUDI.thread_count()
    val () = assertloc(thread_count = 1)
in
    (* XXX add thread usage *)
    task(i2u(0))
end
