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
    val ~$CLOUDI.Ptr(_, _) = request_info
    val @(http_qs, http_qs_size) = $CLOUDI.info_key_value_parse(request)
    val () = assertloc(http_qs_size >= 2)
    val key = "value"
    var value = stropt_none()
    implement
    array_iforeach$cont<string><stropt>
        (i,
         x,
         env) = if (stropt_is_some(env)) then let
        val _ = stropt_unsome(env)
        val () = env := stropt0_some(x)
    in
        false
    end
    else
        true
    implement
    array_iforeach$fwork<string><stropt>
        (i,
         x,
         env) = if (i % i2sz(2) = i2sz(0) && key = x) then
        env := stropt0_some("")
    else
        ()
    val _ = arrayptr_iforeach_env<string><stropt>(http_qs, http_qs_size, value)
    val xml = if (stropt_is_some(value)) then let
        val xml_before = "<http_test><value>"
        val xml_after = "</value></http_test>"
        val value_str: string = stropt_unsome(value)
        val value_int: int = g0string2int(value_str)
        val value_str_parsed = g0int2string(value_int)
        val value_parsed = (value_str_parsed = value_str)
        val () = strptr_free(value_str_parsed)
    in
        if (value_parsed) then
            strptr2stropt(string0_append3(xml_before, value_str, xml_after))
        else
            stropt_none()
    end
    else
        stropt_none()
    val () = arrayptr_free(http_qs)
    val response_info = $CLOUDI.info_key_value_new1(Some_vt(true),
        "content-type", "text/xml; charset=utf-8")
in
    $CLOUDI.ResponseInfo(response_info, $CLOUDI.stropt2free(xml,
        "<http_test><error>no value specified</error></http_test>"))
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
    var state_value: int = 0
in
    case+ $CLOUDI.new(thread_index, state_value, true) of
      | ~$CLOUDI.Ok(api) => let
        val uint_0 = i2u(0)
        val () = case+ $CLOUDI.subscribe_count(api, "ats2.xml/get") of
          | ~$CLOUDI.Ok(uint_0) =>
            ()
          | ~$CLOUDI.Error(status) =>
            assertloc(status = 0)
        val () = case+ $CLOUDI.subscribe(api, "ats2.xml/get", request) of
          | ~$CLOUDI.Ok(_) =>
            ()
          | ~$CLOUDI.Error(status) =>
            assertloc(status = 0)
        val uint_1 = i2u(1)
        val () = case+ $CLOUDI.subscribe_count(api, "ats2.xml/get") of
          | ~$CLOUDI.Ok(uint_1) =>
            ()
          | ~$CLOUDI.Error(status) =>
            assertloc(status = 0)
        val () = case+ $CLOUDI.poll(api, ~1) of
          | ~$CLOUDI.Ok(_) =>
            println!("terminate http_req ats2")
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

