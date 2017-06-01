(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  MIT License

  Copyright (c) 2017 Michael Truog <mjtruog at gmail dot com>

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

let request type_ name pattern _ request timeout _ trans_id pid _ api =
  let http_qs = Cloudi.info_key_value_parse request in
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
  match Cloudi.api thread_index () with
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

