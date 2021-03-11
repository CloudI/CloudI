(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  MIT License

  Copyright (c) 2017-2021 Michael Truog <mjtruog at protonmail dot com>

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

let request
  request_type _ _ request_info request timeout priority trans_id pid _ api =
  match unpack_uint32_native request with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok (i0) ->
    let i1 =
      if i0 = 1073741823 then
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
      request_type destination request_info request_new
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

