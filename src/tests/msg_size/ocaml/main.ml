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

let destination = "/tests/msg_size/erlang"

let unpack_uint32_native binary : (int, string) result =
  let byte0 = int_of_char binary.[if Sys.big_endian then 0 else 3]
  and byte1 = int_of_char binary.[if Sys.big_endian then 1 else 2]
  and byte2 = int_of_char binary.[if Sys.big_endian then 2 else 1]
  and byte3 = int_of_char binary.[if Sys.big_endian then 3 else 0] in
  if byte0 > max_int lsr 24 then
    (* 32 bit system *)
    Error ("ocaml int overflow")
  else
    Ok (
      (byte0 lsl 24) lor (
        (byte1 lsl 16) lor (
          (byte2 lsl 8) lor byte3
        )
      )
    )

let pack_uint32_native (value : int) : string =
  let byte0 = (value lsr 24) land 0xff
  and byte1 = (value lsr 16) land 0xff
  and byte2 = (value lsr 8) land 0xff
  and byte3 = value land 0xff
  and binary = Bytes.create 4 in
  Bytes.set binary (if Sys.big_endian then 0 else 3) (char_of_int byte0) ;
  Bytes.set binary (if Sys.big_endian then 1 else 2) (char_of_int byte1) ;
  Bytes.set binary (if Sys.big_endian then 2 else 1) (char_of_int byte2) ;
  Bytes.set binary (if Sys.big_endian then 3 else 0) (char_of_int byte3) ;
  Bytes.to_string binary

let request type_ _ _ request_info request timeout priority trans_id pid _ api =
  match unpack_uint32_native request with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok (i0) ->
    let i1 =
      if i0 = 4294967295 then
        0
      else
        i0 + 1
    in
    let request_new =
      (pack_uint32_native i1) ^
      (String.sub request 4 ((String.length request) - 4))
    in
    print_endline (
      "forward #" ^ (string_of_int i1) ^ " ocaml to " ^ destination ^
      " (with timeout " ^ (string_of_int timeout) ^ " ms)"
    ) ;
    match Cloudi.forward_ api
      type_ destination request_info request_new
      timeout priority trans_id pid with
    | Error (error) ->
      Cloudi.NullError (error)
    | Ok _ ->
      Cloudi.Null

let task thread_index =
  match Cloudi.api thread_index () with
  | Error (error) ->
    prerr_endline error
  | Ok (api) ->
    match Cloudi.subscribe api "ocaml" request with
    | Error (error) ->
      prerr_endline error
    | Ok _ ->
      match Cloudi.poll api (-1) with
      | Error (error) ->
        prerr_endline error
      | Ok _ ->
        print_endline "terminate msg_size ocaml"

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

