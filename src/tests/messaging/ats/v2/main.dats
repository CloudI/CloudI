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

fn
sequence1_abcd_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/b/c/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test1")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_abcd
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
    $CLOUDI.callback_attach(sequence1_abcd_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_abc__ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/b/c/*")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test2" || request_str = "test3")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_abc_
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
    $CLOUDI.callback_attach(sequence1_abc__ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_ab_d_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/b/*/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test4" || request_str = "test5")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_ab_d
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
    $CLOUDI.callback_attach(sequence1_ab_d_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_a_cd_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/*/c/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test6" || request_str = "test7")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_a_cd
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
    $CLOUDI.callback_attach(sequence1_a_cd_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1__bcd_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "*/b/c/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test8" || request_str = "test9")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1__bcd
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
    $CLOUDI.callback_attach(sequence1__bcd_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_ab___ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/b/*")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test10")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_ab__
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
    $CLOUDI.callback_attach(sequence1_ab___ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_a__d_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/*/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test11")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_a__d
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
    $CLOUDI.callback_attach(sequence1_a__d_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1___cd_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "*/c/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test12")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1___cd
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
    $CLOUDI.callback_attach(sequence1___cd_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1_a____ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "a/*")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test13")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_a___
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
    $CLOUDI.callback_attach(sequence1_a____ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1____d_ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "*/d")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test14")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1____d
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
    $CLOUDI.callback_attach(sequence1____d_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence1______ats
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
    val pattern_expected = string0_append($CLOUDI.prefix_(api), "*")
    val () = assertloc(pattern_expected = pattern)
    val () = strptr_free(pattern_expected)
    val request_str = $CLOUDI.memory2string(request)
    val () = assertloc(request_str = "test15")
    val response = $CLOUDI.memory2free(request)
in
    $CLOUDI.Response(response)
end

fn
sequence1_____
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
    $CLOUDI.callback_attach(sequence1______ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
send_async
    (api: !$CLOUDI.instance(state_type),
     suffix: string,
     request_str: string):
    $CLOUDI.trans_id = let
    val name = strptr2string(string0_append($CLOUDI.prefix_(api), suffix))
    val request = $CLOUDI.string2read(request_str)
in
    case+ $CLOUDI.send_async(api, name, request,
                             None_vt(), None_vt(), None_vt()) of
      | ~$CLOUDI.Ok(trans_id) =>
        trans_id
      | ~$CLOUDI.Error(status) => let
        val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
    in
        $raise $CLOUDI.FatalError
    end
end

fn
sequence1_ats
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
    fun
    wait
        (api: !$CLOUDI.instance(state_type)):
        void = case+ $CLOUDI.recv_async(api, Some_vt(i2u(1000)),
                                        None_vt(), None_vt()) of
      | ~$CLOUDI.Ok(@(response_info_wait, response_wait, trans_id_wait)) => let
        val done = ($CLOUDI.memory2string(response_wait) <> "end")
        val ~$CLOUDI.Ptr(_, _) = response_info_wait
        val ~$CLOUDI.Ptr(_, _) = response_wait
        val () = $CLOUDI.trans_id_free(trans_id_wait)
    in
        if (done) then
            ()
        else
            wait(api)
    end
      | ~$CLOUDI.Error(status) => let
        val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
    in
        $raise $CLOUDI.FatalError
    end
    val () = wait(api)
    val ~$CLOUDI.Ptr(_, _) = request_info
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val () = println!("messaging sequence1 start ats2 (", request_str, ")")
    val test1_id = send_async(api, "a/b/c/d", "test1")
    val test2_id = send_async(api, "a/b/c/z", "test2")
    val test3_id = send_async(api, "a/b/c/dd", "test3")
    val test4_id = send_async(api, "a/b/z/d", "test4")
    val test5_id = send_async(api, "a/b/cc/d", "test5")
    val test6_id = send_async(api, "a/z/c/d", "test6")
    val test7_id = send_async(api, "a/bb/c/d", "test7")
    val test8_id = send_async(api, "z/b/c/d", "test8")
    val test9_id = send_async(api, "aa/b/c/d", "test9")
    val test10_id = send_async(api, "a/b/czd", "test10")
    val test11_id = send_async(api, "a/bzc/d", "test11")
    val test12_id = send_async(api, "azb/c/d", "test12")
    val test13_id = send_async(api, "a/bzczd", "test13")
    val test14_id = send_async(api, "azbzc/d", "test14")
    val test15_id = send_async(api, "azbzczd", "test15")
    (* n.b., depends on cloudi_core_i_constants.hrl having
       RECV_ASYNC_STRATEGY == recv_async_select_oldest *)
    fn
    recv_async_wait
        (api: !$CLOUDI.instance(state_type),
         trans_id_wait: !$CLOUDI.trans_id):
        void = let
        val trans_id_recv = $CLOUDI.trans_id_copy(trans_id_wait)
    in
        case+ $CLOUDI.recv_async(api, None_vt(),
                                 Some_vt(trans_id_recv),
                                 Some_vt(false)) of
          | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
            val ~$CLOUDI.Ptr(_, _) = response_info
            val ~$CLOUDI.Ptr(_, _) = response
            val () = assertloc(trans_id_wait = trans_id)
            val () = $CLOUDI.trans_id_free(trans_id)
        in
            ()
        end
          | ~$CLOUDI.Error(status) => let
            val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
        in
            $raise $CLOUDI.FatalError
        end
    end
    fn
    recv_async_assert
        (api: !$CLOUDI.instance(state_type),
         trans_id_assert: $CLOUDI.trans_id,
         response_assert: string):
        void = case+ $CLOUDI.recv_async(api, None_vt(),
                                        None_vt(), None_vt()) of
      | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
        val ~$CLOUDI.Ptr(_, _) = response_info
        val response_str = $CLOUDI.memory2string(response)
        val ~$CLOUDI.Ptr(_, _) = response
        val () = assertloc(trans_id_assert = trans_id)
        val () = $CLOUDI.trans_id_free(trans_id)
        val () = $CLOUDI.trans_id_free(trans_id_assert)
    in
        assertloc(response_assert = response_str)
    end
      | ~$CLOUDI.Error(status) => let
        val () = $CLOUDI.trans_id_free(trans_id_assert)
        val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
    in
        $raise $CLOUDI.FatalError
    end
    val () = recv_async_wait(api, test1_id)
    val () = recv_async_assert(api, test1_id, "test1")
    val () = recv_async_wait(api, test2_id)
    val () = recv_async_assert(api, test2_id, "test2")
    val () = recv_async_wait(api, test3_id)
    val () = recv_async_assert(api, test3_id, "test3")
    val () = recv_async_wait(api, test4_id)
    val () = recv_async_assert(api, test4_id, "test4")
    val () = recv_async_wait(api, test5_id)
    val () = recv_async_assert(api, test5_id, "test5")
    val () = recv_async_wait(api, test6_id)
    val () = recv_async_assert(api, test6_id, "test6")
    val () = recv_async_wait(api, test7_id)
    val () = recv_async_assert(api, test7_id, "test7")
    val () = recv_async_wait(api, test8_id)
    val () = recv_async_assert(api, test8_id, "test8")
    val () = recv_async_wait(api, test9_id)
    val () = recv_async_assert(api, test9_id, "test9")
    val () = recv_async_wait(api, test10_id)
    val () = recv_async_assert(api, test10_id, "test10")
    val () = recv_async_wait(api, test11_id)
    val () = recv_async_assert(api, test11_id, "test11")
    val () = recv_async_wait(api, test12_id)
    val () = recv_async_assert(api, test12_id, "test12")
    val () = recv_async_wait(api, test13_id)
    val () = recv_async_assert(api, test13_id, "test13")
    val () = recv_async_wait(api, test14_id)
    val () = recv_async_assert(api, test14_id, "test14")
    val () = recv_async_wait(api, test15_id)
    val () = recv_async_assert(api, test15_id, "test15")
    val () = println!("messaging sequence1 end ats2 (", request_str, ")")
    (* start sequence2 *)
    val trans_id_next = send_async(api, "sequence1", request_str)
    val () = $CLOUDI.trans_id_free(trans_id_next)
in
    $CLOUDI.Response($CLOUDI.StringLiteral("end"))
end

fn
sequence1
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
    $CLOUDI.callback_attach(sequence1_ats,
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
    case+ $CLOUDI.new(thread_index, state_value, false) of
      | ~$CLOUDI.Ok(api) => let
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/b/c/d",
                                                sequence1_abcd)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/b/c/*",
                                                sequence1_abc_)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/b/*/d",
                                                sequence1_ab_d)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/*/c/d",
                                                sequence1_a_cd)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "*/b/c/d",
                                                sequence1__bcd)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/b/*",
                                                sequence1_ab__)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/*/d",
                                                sequence1_a__d)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "*/c/d",
                                                sequence1___cd)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "a/*",
                                                sequence1_a___)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "*/d",
                                                sequence1____d)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "*",
                                                sequence1_____)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "sequence1",
                                                sequence1)
        val () = if (thread_index = i2u(0)) then let
            (* start sequence1 *)
            val trans_id = send_async(api, "sequence1", "1")
        in
            $CLOUDI.trans_id_free(trans_id)
        end
        else
            ()
        val () = case+ $CLOUDI.poll(api, ~1) of
          | ~$CLOUDI.Ok(_) =>
            println!("terminate messaging ats2")
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

