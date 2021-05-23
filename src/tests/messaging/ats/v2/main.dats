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
    val name_ptr = string0_append($CLOUDI.prefix_(api), suffix)
    val name = $UNSAFE.strptr2string(name_ptr)
    val request = $CLOUDI.string2read(request_str)
in
    case+ $CLOUDI.send_async(api, name, request,
                             None_vt(), None_vt(), None_vt()) of
      | ~$CLOUDI.Ok(trans_id) => let
        val () = strptr_free(name_ptr)
    in
        trans_id
    end
      | ~$CLOUDI.Error(status) => let
        val () = strptr_free(name_ptr)
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
    val ~$CLOUDI.Ptr(_, _) = request_info
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
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
    val () = println!("messaging sequence1 start ats2 (", request_str, ")")
    val test1_id = let
        val test1_id_temp = send_async(api, "a/b/c/d", "test1")
    in
        $CLOUDI.trans_id_store(test1_id_temp)
    end
    val test2_id = let
        val test2_id_temp = send_async(api, "a/b/c/z", "test2")
    in
        $CLOUDI.trans_id_store(test2_id_temp)
    end
    val test3_id = let
        val test3_id_temp = send_async(api, "a/b/c/dd", "test3")
    in
        $CLOUDI.trans_id_store(test3_id_temp)
    end
    val test4_id = let
        val test4_id_temp = send_async(api, "a/b/z/d", "test4")
    in
        $CLOUDI.trans_id_store(test4_id_temp)
    end
    val test5_id = let
        val test5_id_temp = send_async(api, "a/b/cc/d", "test5")
    in
        $CLOUDI.trans_id_store(test5_id_temp)
    end
    val test6_id = let
        val test6_id_temp = send_async(api, "a/z/c/d", "test6")
    in
        $CLOUDI.trans_id_store(test6_id_temp)
    end
    val test7_id = let
        val test7_id_temp = send_async(api, "a/bb/c/d", "test7")
    in
        $CLOUDI.trans_id_store(test7_id_temp)
    end
    val test8_id = let
        val test8_id_temp = send_async(api, "z/b/c/d", "test8")
    in
        $CLOUDI.trans_id_store(test8_id_temp)
    end
    val test9_id = let
        val test9_id_temp = send_async(api, "aa/b/c/d", "test9")
    in
        $CLOUDI.trans_id_store(test9_id_temp)
    end
    val test10_id = let
        val test10_id_temp = send_async(api, "a/b/czd", "test10")
    in
        $CLOUDI.trans_id_store(test10_id_temp)
    end
    val test11_id = let
        val test11_id_temp = send_async(api, "a/bzc/d", "test11")
    in
        $CLOUDI.trans_id_store(test11_id_temp)
    end
    val test12_id = let
        val test12_id_temp = send_async(api, "azb/c/d", "test12")
    in
        $CLOUDI.trans_id_store(test12_id_temp)
    end
    val test13_id = let
        val test13_id_temp = send_async(api, "a/bzczd", "test13")
    in
        $CLOUDI.trans_id_store(test13_id_temp)
    end
    val test14_id = let
        val test14_id_temp = send_async(api, "azbzc/d", "test14")
    in
        $CLOUDI.trans_id_store(test14_id_temp)
    end
    val test15_id = let
        val test15_id_temp = send_async(api, "azbzczd", "test15")
    in
        $CLOUDI.trans_id_store(test15_id_temp)
    end
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
        in
            $CLOUDI.trans_id_free(trans_id)
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
    val trans_id_next = send_async(api, "sequence2", request_str)
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
sequence2_e1_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("1"))
end

fn
sequence2_e1
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
    $CLOUDI.callback_attach(sequence2_e1_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e2_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("2"))
end

fn
sequence2_e2
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
    $CLOUDI.callback_attach(sequence2_e2_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e3_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("3"))
end

fn
sequence2_e3
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
    $CLOUDI.callback_attach(sequence2_e3_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e4_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("4"))
end

fn
sequence2_e4
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
    $CLOUDI.callback_attach(sequence2_e4_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e5_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("5"))
end

fn
sequence2_e5
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
    $CLOUDI.callback_attach(sequence2_e5_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e6_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("6"))
end

fn
sequence2_e6
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
    $CLOUDI.callback_attach(sequence2_e6_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e7_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("7"))
end

fn
sequence2_e7
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
    $CLOUDI.callback_attach(sequence2_e7_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_e8_ats
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
in
    $CLOUDI.Response($CLOUDI.StringLiteral("8"))
end

fn
sequence2_e8
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
    $CLOUDI.callback_attach(sequence2_e8_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence2_ats
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
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val () = println!("messaging sequence2 start ats2 (", request_str, ")")
    fun
    recv_asyncs_loop
        (api: !$CLOUDI.instance(state_type)):
        void = let
        val name_mcast_ptr = string0_append($CLOUDI.prefix_(api), "e")
        val name_mcast = $UNSAFE.strptr2string(name_mcast_ptr)
        val request_mcast = $CLOUDI.string2read("")
    in
        (* the sending process is excluded from the services that receive
           the asynchronous message, so in this case, the receiving thread
           will not be called, despite the fact it has subscribed to "e",
           to prevent a process (in this case thread) from deadlocking
           with itself. *)
        case+ $CLOUDI.mcast_async(api, name_mcast, request_mcast,
                                  None_vt(), None_vt(), None_vt()) of
          | ~$CLOUDI.Ok(@(trans_ids, trans_ids_size)) => let
            val () = strptr_free(name_mcast_ptr)
        in
            (* 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
               since 1 thread is sending the mcast_async, so 3 * 8 == 24 *)
            if (trans_ids_size = i2sz(24)) then let
                val () = $CLOUDI.trans_ids_store(trans_ids, trans_ids_size)
                val e_check = arrayptr_make_elt<int>(trans_ids_size, 0)
                fun recv_loop {i,ni:nat | i <= ni} .<i>.
                    (api: !$CLOUDI.instance(state_type),
                     p_ids: ptr,
                     p_e_check: ptr,
                     p_i: size_t(i),
                     p_n: size_t(ni)):<fun1>
                    void = if (p_i > 0) then let
                    val trans_id = $UNSAFE.ptr0_get<$CLOUDI.trans_id>(p_ids)
                    val () = case+ $CLOUDI.recv_async(api, None_vt(),
                                                      Some_vt(trans_id),
                                                      None_vt()) of
                      | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
                        val ~$CLOUDI.Ptr(_, _) = response_info
                        val response_str = $CLOUDI.memory2string(response)
                        val response_int = g0string2int(response_str)
                        val ~$CLOUDI.Ptr(_, _) = response
                        val () = $CLOUDI.trans_id_free(trans_id)
                    in
                        $UNSAFE.ptr0_set<int>(p_e_check, response_int)
                    end
                      | ~$CLOUDI.Error(status) => let
                        val () = fprintln!(stderr_ref,
                                           "error ", status, ": ", $mylocation)
                    in
                        $raise $CLOUDI.FatalError
                    end
                    val p_ids_next = ptr0_succ<$CLOUDI.trans_id>(p_ids)
                    val p_e_check_next = ptr0_succ<int>(p_e_check)
                in
                    recv_loop(api, p_ids_next, p_e_check_next,
                              p_i - i2sz(1), p_n)
                end
                else
                    ()
                val () = recv_loop(api, ptrcast(trans_ids), ptrcast(e_check),
                                   trans_ids_size, trans_ids_size)
                val () = arrayptr_free($UNSAFE.castvwtp0(trans_ids))
                val () = arrayptr_quicksort<int>(e_check, trans_ids_size)
                fun assert_loop {i,ni:nat | i <= ni} .<i>.
                    (p_e_check: ptr,
                     p_i: size_t(i),
                     p_n: size_t(ni)):<!exn>
                    void = if (p_i > 0) then let
                    val e_check = $UNSAFE.ptr0_get<int>(p_e_check)
                    val expected = i2sz(1) + (p_n - p_i) / i2sz(3)
                    val () = assertloc(expected = e_check)
                    val p_e_check_next = ptr0_succ<int>(p_e_check)
                in
                    assert_loop(p_e_check_next, p_i - i2sz(1), p_n)
                end
                else
                    ()
                val () = assert_loop(ptrcast(e_check),
                                     trans_ids_size, trans_ids_size)
                val () = arrayptr_free(e_check)
            in
                ()
            end
            else let
                val () = $CLOUDI.trans_ids_free(trans_ids, trans_ids_size)
                val count = 4 - sz2i(trans_ids_size) / 8
                val () = println!("Waiting for ", count,
                                  " services to initialize")
            in
                case+ $CLOUDI.recv_async(api, Some_vt(i2u(1000)),
                                         None_vt(), None_vt()) of
                  | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
                    val ~$CLOUDI.Ptr(_, _) = response_info
                    val ~$CLOUDI.Ptr(_, _) = response
                    val () = assertloc(iseqz(trans_id))
                    val () = $CLOUDI.trans_id_free(trans_id)
                in
                    recv_asyncs_loop(api)
                end
                  | ~$CLOUDI.Error(status) => let
                    val () = fprintln!(stderr_ref,
                                       "error ", status, ": ", $mylocation)
                in
                    $raise $CLOUDI.FatalError
                end
            end
        end
          | ~$CLOUDI.Error(status) => let
            val () = strptr_free(name_mcast_ptr)
            val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
        in
            $raise $CLOUDI.FatalError
        end
    end
    val () = recv_asyncs_loop(api)
    val () = println!("messaging sequence2 end ats2 (", request_str, ")")
    (* start sequence3 *)
    val trans_id_next = send_async(api, "sequence3", request_str)
    val () = $CLOUDI.trans_id_free(trans_id_next)
in
    $CLOUDI.Response($CLOUDI.StringLiteral("end"))
end

fn
sequence2
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
    $CLOUDI.callback_attach(sequence2_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence3_f1_ats
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
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val request_i = g0string2int(request_str)
in
    if (request_i = 4) then
        $CLOUDI.Response($CLOUDI.StringLiteral("done"))
    else let
        val name_next = string0_append($CLOUDI.prefix_(api), "f2")
        val request_new = g0int2string(request_i + 2) (* two steps forward *)
    in
        $CLOUDI.Forward($CLOUDI.strptr2free(name_next),
                        $CLOUDI.StringLiteral(""),
                        $CLOUDI.strptr2free(request_new))
    end
end

fn
sequence3_f1
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
    $CLOUDI.callback_attach(sequence3_f1_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence3_f2_ats
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
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val request_i = g0string2int(request_str)
    val name_next = string0_append($CLOUDI.prefix_(api), "f1")
    val request_new = g0int2string(request_i - 1) (* one step back *)
in
    $CLOUDI.Forward($CLOUDI.strptr2free(name_next),
                    $CLOUDI.StringLiteral(""),
                    $CLOUDI.strptr2free(request_new))
end

fn
sequence3_f2
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
    $CLOUDI.callback_attach(sequence3_f2_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence3_g1_ats
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
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val response = string0_append(request_str, "suffix")
in
    $CLOUDI.Response($CLOUDI.strptr2free(response))
end

fn
sequence3_g1
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
    $CLOUDI.callback_attach(sequence3_g1_ats,
                            request_type, name_c, pattern_c,
                            request_info_c, request_info_size_c,
                            request_c, request_size_c,
                            timeout_c, priority_c, trans_id_c,
                            pid_c, pid_size_c, state_c, api_c)

fn
sequence3_ats
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
    val request_str = $CLOUDI.memory2string(request)
    val ~$CLOUDI.Ptr(_, _) = request
    val () = println!("messaging sequence3 start ats2 (", request_str, ")")
    val test1_id = send_async(api, "f1", "0")
    val () = case+ $CLOUDI.recv_async(api, None_vt(),
                                      Some_vt(test1_id), None_vt()) of
      | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
        val ~$CLOUDI.Ptr(_, _) = response_info
        val response_str = $CLOUDI.memory2string(response)
        val ~$CLOUDI.Ptr(_, _) = response
        val () = $CLOUDI.trans_id_free(trans_id)
    in
        assertloc(response_str = "done")
    end
      | ~$CLOUDI.Error(status) => let
        val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
    in
        $raise $CLOUDI.FatalError
    end
    val name_send_sync_ptr = string0_append($CLOUDI.prefix_(api), "g1")
    val name_send_sync = $UNSAFE.strptr2string(name_send_sync_ptr)
    val request_send_sync = $CLOUDI.string2read("prefix_")
    val () = case+ $CLOUDI.send_sync(api, name_send_sync, request_send_sync,
                                     None_vt(), None_vt(), None_vt()) of
      | ~$CLOUDI.Ok(@(response_info, response, trans_id)) => let
        val ~$CLOUDI.Ptr(_, _) = response_info
        val response_str = $CLOUDI.memory2string(response)
        val ~$CLOUDI.Ptr(_, _) = response
        val () = $CLOUDI.trans_id_free(trans_id)
    in
        assertloc(response_str = "prefix_suffix")
    end
      | ~$CLOUDI.Error(status) => let
        val () = fprintln!(stderr_ref, "error ", status, ": ", $mylocation)
    in
        $raise $CLOUDI.FatalError
    end
    val () = strptr_free(name_send_sync_ptr)
    val () = println!("messaging sequence3 end ats2 (", request_str, ")")
    (* loop to find any infrequent problems, restart sequence1 *)
    val iteration = g0string2int(request_str) + 1
    val request_next_ptr = g0int2string(iteration)
    val request_next = $UNSAFE.strptr2string(request_next_ptr)
    val trans_id_next = send_async(api, "sequence1", request_next)
    val () = strptr_free(request_next_ptr)
    val () = $CLOUDI.trans_id_free(trans_id_next)
in
    $CLOUDI.Response($CLOUDI.StringLiteral("end"))
end

fn
sequence3
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
    $CLOUDI.callback_attach(sequence3_ats,
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
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e1)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e2)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e3)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e4)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e5)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e6)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e7)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "e",
                                                sequence2_e8)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "sequence2",
                                                sequence2)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "f1",
                                                sequence3_f1)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "f2",
                                                sequence3_f2)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "g1",
                                                sequence3_g1)
        val- ~$CLOUDI.Ok(_) = $CLOUDI.subscribe(api, "sequence3",
                                                sequence3)
        val () = if ($CLOUDI.process_index(api) = i2u(0)) then let
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

