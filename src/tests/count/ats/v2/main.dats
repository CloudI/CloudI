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

vtypedef state_type = int

fn
request
    (request_type: $CLOUDI.request_type,
     name: string,
     pattern: string,
     request_info: $CLOUDI.memory_ptr,
     request: $CLOUDI.memory_ptr,
     timeout: $CLOUDI.timeout,
     priority: $CLOUDI.priority,
     trans_id: !$CLOUDI.trans_id,
     source: !$CLOUDI.memory_ptr,
     state: !$CLOUDI.stateptr(state_type),
     api: !$CLOUDI.instance(state_type)):
    $CLOUDI.response = let
    val ~$CLOUDI.Ptr(_, _) = request_info
    val ~$CLOUDI.Ptr(_, _) = request
    val state_value: int = aptr_get_elt<int>(state) + 1
    val () = println!("count == ", state_value, " ats2")
    val () = aptr_set_elt(state, state_value)
    val response = $CLOUDI.strptr2free(g0int2string(state_value))
in
    $CLOUDI.Response(response)
end

fn
task
    (thread_index: uint):
    void = let
    var state_value: int = 0
in
    case+ $CLOUDI.new(thread_index, state_value, true) of
      | ~$CLOUDI.Ok(api) => let
        implement
        $CLOUDI.subscribe$function<state_type>() = request
        val () = case+ $CLOUDI.subscribe<state_type>(api, "ats2/get") of
          | ~$CLOUDI.Ok(_) =>
            ()
          | ~$CLOUDI.Error(status) =>
            assertloc(status = 0)
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

