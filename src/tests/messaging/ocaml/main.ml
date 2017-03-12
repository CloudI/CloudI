(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  BSD LICENSE
  
  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
      * All advertising materials mentioning features or use of this
        software must display the following acknowledgment:
          This product includes software developed by Michael Truog
      * The name of the author may not be used to endorse or promote
        products derived from this software without specific prior
        written permission
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.
 
 *)

let return api
  type_ name pattern response_info response timeout trans_id pid =
  match Cloudi.return_ api
    type_ name pattern response_info response timeout trans_id pid with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let sequence1_abcd type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/c/d")) ;
  assert (request = "test1") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_abc_ type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/c/*")) ;
  assert ((request = "test2") || (request = "test3")) ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_ab_d type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/*/d")) ;
  assert ((request = "test4") || (request = "test5")) ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_a_cd type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*/c/d")) ;
  assert ((request = "test6") || (request = "test7")) ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1__bcd type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/b/c/d")) ;
  assert ((request = "test8") || (request = "test9")) ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_ab__ type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/b/*")) ;
  assert (request = "test10") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_a__d type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*/d")) ;
  assert (request = "test11") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1___cd type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/c/d")) ;
  assert (request = "test12") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_a___ type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "a/*")) ;
  assert (request = "test13") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1____d type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*/d")) ;
  assert (request = "test14") ;
  return api type_ name pattern "" request timeout trans_id pid

let sequence1_____ type_ name pattern _ request timeout _ trans_id pid _ api =
  assert (pattern = ((Cloudi.prefix api) ^ "*")) ;
  assert (request = "test15") ;
  return api type_ name pattern "" request timeout trans_id pid

let send_async api suffix request =
  match Cloudi.send_async api ((Cloudi.prefix api) ^ suffix) request with
  | Error (error) ->
    prerr_endline error ;
    raise Exit
  | Ok (trans_id) ->
    trans_id

let sequence1 type_ name pattern _ request timeout _ trans_id pid _ api =
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
  print_endline "messaging sequence1 start ocaml" ;
  assert (request = "start") ;
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
  print_endline "messaging sequence1 end ocaml" ;
  (* start sequence2 *)
  let _ = send_async api "sequence2" "start" in
  return api type_ name pattern "" "end" timeout trans_id pid

let sequence2_e1 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "1" timeout trans_id pid

let sequence2_e2 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "2" timeout trans_id pid

let sequence2_e3 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "3" timeout trans_id pid

let sequence2_e4 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "4" timeout trans_id pid

let sequence2_e5 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "5" timeout trans_id pid

let sequence2_e6 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "6" timeout trans_id pid

let sequence2_e7 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "7" timeout trans_id pid

let sequence2_e8 type_ name pattern _ _ timeout _ trans_id pid _ api =
  return api type_ name pattern "" "8" timeout trans_id pid

let sequence2 type_ name pattern _ request timeout _ trans_id pid _ api =
  print_endline "messaging sequence2 start ocaml" ;
  assert (request = "start") ;
  match Cloudi.mcast_async api ((Cloudi.prefix api) ^ "e") " " with
  | Error (error) ->
    prerr_endline error ;
    raise Exit
  | Ok (trans_ids) ->
    let rec recv_asyncs () =
      (* the sending process is excluded from the services that receive
        the asynchronous message, so in this case, the receiving thread
        will not be called, despite the fact it has subscribed to 'e',
        to prevent a process (in this case thread) from deadlocking
        with itself. *)
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
          recv_asyncs ()
        | Ok _ ->
          prerr_endline "invalid!" ;
          raise Exit) ;
    in
    recv_asyncs () ;
    print_endline "messaging sequence2 end ocaml" ;
    (* start sequence3 *)
    let _ = send_async api "sequence3" "start" in
    return api type_ name pattern "" "end" timeout trans_id pid

let forward api
  type_ suffix request_info request timeout priority trans_id pid =
  match Cloudi.forward_ api
    type_ ((Cloudi.prefix api) ^ suffix) request_info request
    timeout priority trans_id pid with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let sequence3_f1
  type_ _ _ request_info request timeout priority trans_id pid _ api =
  let request_i = int_of_string request in
  if request_i = 4 then
    Cloudi.Response ("done")
  else
    let request_new = request_i + 2 in (* two steps forward *)
    forward api
      type_ "f2" request_info (string_of_int request_new)
      timeout priority trans_id pid

let sequence3_f2
  type_ _ _ request_info request timeout priority trans_id pid _ api =
  let request_i = int_of_string request in
  let request_new = request_i - 1 in (* one step back *)
  forward api
    type_ "f1" request_info (string_of_int request_new)
    timeout priority trans_id pid

let sequence3_g1 type_ name pattern _ request timeout _ trans_id pid _ api =
  return api type_ name pattern "" (request ^ "suffix") timeout trans_id pid

let sequence3 type_ name pattern _ request timeout _ trans_id pid _ api =
  print_endline "messaging sequence3 start ocaml" ;
  assert (request = "start") ;
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
      print_endline "messaging sequence3 end ocaml" ;
      (* loop to find any infrequent problems, restart sequence1 *)
      let _ = send_async api "sequence1" "start" in
      return api type_ name pattern "" "end" timeout trans_id pid)
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
  match Cloudi.api thread_index () with
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
        ((Cloudi.prefix api) ^ "sequence1") "start" in
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
    prerr_endline error
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

