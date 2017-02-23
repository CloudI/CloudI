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

let request api type_ name pattern _ request timeout _ trans_id pid =
  let http_qs = Cloudi.request_http_qs_parse request in
  let value = try Some (int_of_string (List.hd (Hashtbl.find http_qs "value")))
  with _ -> None in
  let response = match value with
  | None ->
    "<http_test><error>no value specified</error></http_test>"
  | Some (value) ->
    let s = string_of_int value in
    "<http_test><value>" ^ s ^ "</value></http_test>"
  in
  match Cloudi.return_ api
    type_ name pattern "" response timeout trans_id pid with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let task thread_index =
  match Cloudi.api thread_index with
  | Error (error) ->
    prerr_endline error
  | Ok (api) ->
    match Cloudi.subscribe_count api "ocaml.xml/get" with
    | Error (error) ->
      prerr_endline error
    | Ok (count0) when count0 <> 0 ->
      prerr_endline "subscribe_count <> 0"
    | Ok _ ->
      match Cloudi.subscribe api "ocaml.xml/get" request with
      | Error (error) ->
        prerr_endline error
      | Ok _ ->
        match Cloudi.subscribe_count api "ocaml.xml/get" with
        | Error (error) ->
          prerr_endline error
        | Ok (count1) when count1 <> 1 ->
          prerr_endline "subscribe_count <> 1"
        | Ok _ ->
          match Cloudi.poll api (-1) with
          | Error (error) ->
            prerr_endline error
          | Ok _ ->
            print_endline "terminate http_req ocaml"

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

