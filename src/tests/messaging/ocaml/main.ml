(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  MIT License

  Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>

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

let return api
  request_type name pattern response_info response timeout trans_id source =
  match Cloudi.return_ api
    request_type name pattern response_info response
    timeout trans_id source with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let sequence1_abcd
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/c/d")) ;
  assert (request = "test1") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_abc_
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/c/*")) ;
  assert ((request = "test2") || (request = "test3")) ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_ab_d
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/*/d")) ;
  assert ((request = "test4") || (request = "test5")) ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_a_cd
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*/c/d")) ;
  assert ((request = "test6") || (request = "test7")) ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1__bcd
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/b/c/d")) ;
  assert ((request = "test8") || (request = "test9")) ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_ab__
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/*")) ;
  assert (request = "test10") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_a__d
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*/d")) ;
  assert (request = "test11") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1___cd
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/c/d")) ;
  assert (request = "test12") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_a___
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*")) ;
  assert (request = "test13") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1____d
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/d")) ;
  assert (request = "test14") ;
  return api request_type name pattern "" request timeout trans_id source

let sequence1_____
  request_type name pattern _ request timeout _ trans_id source _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*")) ;
  assert (request = "test15") ;
  return api request_type name pattern "" request timeout trans_id source

let send_async api suffix request =
  match Cloudi.send_async api ((Cloudi.prefix api) ^ suffix) request with
  | Error (error) ->
    prerr_endline error ;
    raise Exit
  | Ok (trans_id) ->
    trans_id

let sequence1
  request_type name pattern _ request timeout _ trans_id source _ api =
  let rec wait () =
    (* consume all the "end" responses from all sequences handled
       by this service *)
    match Cloudi.recv_async api ~timeout:1000 with
    | Error (error) ->
      prerr_endline error ;
      raise Exit
    | Ok ((_, response_waited, _)) when response_waited = "end" ->
      wait ()
    | Ok _ ->
      ()
  in
  wait () ;
  print_endline ("messaging sequence1 start ocaml (" ^ request ^ ")") ;
  let test1_id = send_async api "a/b/c/d" "test1"
  and test2_id = send_async api "a/b/c/z" "test2"
  and test3_id = send_async api "a/b/c/dd" "test3"
  and test4_id = send_async api "a/b/z/d" "test4"
  and test5_id = send_async api "a/b/cc/d" "test5"
  and test6_id = send_async api "a/z/c/d" "test6"
  and test7_id = send_async api "a/bb/c/d" "test7"
  and test8_id = send_async api "z/b/c/d" "test8"
  and test9_id = send_async api "aa/b/c/d" "test9"
  and test10_id = send_async api "a/b/czd" "test10"
  and test11_id = send_async api "a/bzc/d" "test11"
  and test12_id = send_async api "azb/c/d" "test12"
  and test13_id = send_async api "a/bzczd" "test13"
  and test14_id = send_async api "azbzc/d" "test14"
  and test15_id = send_async api "azbzczd" "test15"
  in
  (* n.b., depends on cloudi_core_i_constants.hrl having
     RECV_ASYNC_STRATEGY == recv_async_select_oldest *)
  let recv_async_wait api trans_id_wait =
    match Cloudi.recv_async api ~trans_id:trans_id_wait ~consume:false with
    | Error (error) ->
      prerr_endline error ;
      raise Exit
    | Ok ((_, _, trans_id_waited)) when trans_id_wait = trans_id_waited ->
      ()
    | Ok _ ->
      prerr_endline "timeout!" ;
      raise Exit
  and recv_async_assert api trans_id_assert response_assert =
    match Cloudi.recv_async api with
    | Error (error) ->
      prerr_endline error ;
      raise Exit
    | Ok ((_, response_asserted, trans_id_asserted))
      when trans_id_assert = trans_id_asserted ->
      assert (response_assert = response_asserted) ;
      ()
    | Ok _ ->
      prerr_endline "timeout!" ;
      raise Exit
  in
  recv_async_wait api test1_id ;
  recv_async_assert api test1_id "test1" ;
  recv_async_wait api test2_id ;
  recv_async_assert api test2_id "test2" ;
  recv_async_wait api test3_id ;
  recv_async_assert api test3_id "test3" ;
  recv_async_wait api test4_id ;
  recv_async_assert api test4_id "test4" ;
  recv_async_wait api test5_id ;
  recv_async_assert api test5_id "test5" ;
  recv_async_wait api test6_id ;
  recv_async_assert api test6_id "test6" ;
  recv_async_wait api test7_id ;
  recv_async_assert api test7_id "test7" ;
  recv_async_wait api test8_id ;
  recv_async_assert api test8_id "test8" ;
  recv_async_wait api test9_id ;
  recv_async_assert api test9_id "test9" ;
  recv_async_wait api test10_id ;
  recv_async_assert api test10_id "test10" ;
  recv_async_wait api test11_id ;
  recv_async_assert api test11_id "test11" ;
  recv_async_wait api test12_id ;
  recv_async_assert api test12_id "test12" ;
  recv_async_wait api test13_id ;
  recv_async_assert api test13_id "test13" ;
  recv_async_wait api test14_id ;
  recv_async_assert api test14_id "test14" ;
  recv_async_wait api test15_id ;
  recv_async_assert api test15_id "test15" ;
  print_endline ("messaging sequence1 end ocaml (" ^ request ^ ")") ;
  (* start sequence2 *)
  let _ = send_async api "sequence2" request in
  return api request_type name pattern "" "end" timeout trans_id source

let sequence2_e1
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "1" timeout trans_id source

let sequence2_e2
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "2" timeout trans_id source

let sequence2_e3
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "3" timeout trans_id source

let sequence2_e4
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "4" timeout trans_id source

let sequence2_e5
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "5" timeout trans_id source

let sequence2_e6
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "6" timeout trans_id source

let sequence2_e7
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "7" timeout trans_id source

let sequence2_e8
  request_type name pattern _ _ timeout _ trans_id source _ api =
  return api request_type name pattern "" "8" timeout trans_id source

let sequence2
  request_type name pattern _ request timeout _ trans_id source _ api =
  print_endline ("messaging sequence2 start ocaml (" ^ request ^ ")") ;
  let rec recv_asyncs_loop () =
    (* the sending process is excluded from the services that receive
       the asynchronous message, so in this case, the receiving thread
       will not be called, despite the fact it has subscribed to "e",
       to prevent a process (in this case thread) from deadlocking
       with itself. *)
    match Cloudi.mcast_async api ((Cloudi.prefix api) ^ "e") " " with
    | Error (error) ->
      prerr_endline error ;
      raise Exit
    | Ok (trans_ids) ->
      let rec loop i l =
        if i = (Array.length trans_ids) then
          l
        else
          match Cloudi.recv_async api ~trans_id:trans_ids.(i) with
          | Error (error) ->
            prerr_endline error ;
            raise Exit
          | Ok ((_, j, trans_id_loop)) when trans_id_loop = trans_ids.(i) ->
            loop (i + 1) ([j] @ l)
          | Ok _ ->
            prerr_endline "timeout!" ;
            raise Exit
      in
      let e_check_list = loop 0 [] in
      (* 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
         since 1 thread is sending the mcast_async, so 3 * 8 == 24 *)
      if (Array.length trans_ids) = 24 then
        assert ((String.concat "" (List.sort String.compare e_check_list)) =
                "111222333444555666777888")
      else (
        let count = 4 - (Array.length trans_ids) / 8 in
        print_endline (
          "Waiting for " ^ (string_of_int count) ^ " services to initialize") ;
        match Cloudi.recv_async api ~timeout:1000 with
        | Error (error) ->
          prerr_endline error ;
          raise Exit
        | Ok ((_, _, trans_id_wait))
          when trans_id_wait = Cloudi.trans_id_null ->
          recv_asyncs_loop ()
        | Ok _ ->
          prerr_endline "invalid!" ;
          raise Exit) ;
  in
  recv_asyncs_loop () ;
  print_endline ("messaging sequence2 end ocaml (" ^ request ^ ")") ;
  (* start sequence3 *)
  let _ = send_async api "sequence3" request in
  return api request_type name pattern "" "end" timeout trans_id source

let forward api
  request_type suffix request_info request timeout priority trans_id source =
  match Cloudi.forward_ api
    request_type ((Cloudi.prefix api) ^ suffix) request_info request
    timeout priority trans_id source with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let sequence3_f1
  request_type _ _ request_info request timeout priority trans_id source _ api =
  let request_i = int_of_string request in
  if request_i = 4 then
    Cloudi.Response ("done")
  else
    let request_new = request_i + 2 in (* two steps forward *)
    forward api
      request_type "f2" request_info (string_of_int request_new)
      timeout priority trans_id source

let sequence3_f2
  request_type _ _ request_info request timeout priority trans_id source _ api =
  let request_i = int_of_string request in
  let request_new = request_i - 1 in (* one step back *)
  forward api
    request_type "f1" request_info (string_of_int request_new)
    timeout priority trans_id source

let sequence3_g1
  request_type name pattern _ request timeout _ trans_id source _ api =
  return api
    request_type name pattern "" (request ^ "suffix") timeout trans_id source

let sequence3
  request_type name pattern _ request timeout _ trans_id source _ api =
  print_endline ("messaging sequence3 start ocaml (" ^ request ^ ")") ;
  let test1_id = send_async api "f1" "0" in
  match Cloudi.recv_async api ~trans_id:test1_id with
  | Error (error) ->
    prerr_endline error ;
    raise Exit
  | Ok ((_, test1_check, test1_id_check)) when test1_id_check = test1_id -> (
    assert (test1_check = "done") ;
    match Cloudi.send_sync api ((Cloudi.prefix api) ^ "g1") "prefix_" with
    | Error (error) ->
      prerr_endline error ;
      raise Exit
    | Ok ((_, test2_check, _)) ->
      assert (test2_check = "prefix_suffix") ;
      print_endline ("messaging sequence3 end ocaml (" ^ request ^ ")") ;
      (* loop to find any infrequent problems, restart sequence1 *)
      let iteration = (int_of_string request) + 1 in
      let iteration_new =
        if iteration = max_int then
          0
        else
          iteration
      in
      let _ = send_async api "sequence1" (string_of_int iteration_new) in
      return api request_type name pattern "" "end" timeout trans_id source)
  | Ok _ ->
    prerr_endline "timeout!" ;
    raise Exit

let subscribe api suffix f =
  match Cloudi.subscribe api suffix f with
  | Error (error) ->
    prerr_endline error ;
    raise Exit
  | Ok _ ->
    ()

let task thread_index =
  match Cloudi.api thread_index () ~terminate_return_value:false with
  | Error (error) ->
    prerr_endline error
  | Ok (api) ->
    subscribe api "a/b/c/d" sequence1_abcd ;
    subscribe api "a/b/c/*" sequence1_abc_ ;
    subscribe api "a/b/*/d" sequence1_ab_d ;
    subscribe api "a/*/c/d" sequence1_a_cd ;
    subscribe api "*/b/c/d" sequence1__bcd ;
    subscribe api "a/b/*"   sequence1_ab__ ;
    subscribe api "a/*/d"   sequence1_a__d ;
    subscribe api "*/c/d"   sequence1___cd ;
    subscribe api "a/*"     sequence1_a___ ;
    subscribe api "*/d"     sequence1____d ;
    subscribe api "*"       sequence1_____ ;
    subscribe api "sequence1" sequence1 ;
    subscribe api "e" sequence2_e1 ;
    subscribe api "e" sequence2_e2 ;
    subscribe api "e" sequence2_e3 ;
    subscribe api "e" sequence2_e4 ;
    subscribe api "e" sequence2_e5 ;
    subscribe api "e" sequence2_e6 ;
    subscribe api "e" sequence2_e7 ;
    subscribe api "e" sequence2_e8 ;
    subscribe api "sequence2" sequence2 ;
    subscribe api "f1" sequence3_f1 ;
    subscribe api "f2" sequence3_f2 ;
    subscribe api "g1" sequence3_g1 ;
    subscribe api "sequence3" sequence3 ; (
    if thread_index = 0 then
      (* start sequence1 *)
      let _ = Cloudi.send_async api
        ((Cloudi.prefix api) ^ "sequence1") "1" in
      ()) ;
    match Cloudi.poll api (-1) with
    | Error (error) ->
      prerr_endline error
    | Ok _ ->
      print_endline "terminate messaging ocaml"

let () = 
  Printexc.record_backtrace true ;
  match Cloudi.thread_count () with
  | Error (error) ->
    prerr_endline error ;
    exit 1
  | Ok (thread_count) ->
    let rec loop thread_index threads =
      if thread_index = thread_count then
        threads
      else
        let thread = Thread.create task thread_index in
        loop (thread_index + 1) ([thread] @ threads)
    and wait = function
      | [] ->
        ()
      | thread::threads ->
        Thread.join thread ;
        wait threads
    in
    wait (loop 0 [])

