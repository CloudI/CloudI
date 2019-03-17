(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  MIT License

  Copyright (c) 2017-2019 Michael Truog <mjtruog at protonmail dot com>

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

module ServiceState = struct
  type t = {
      mutable count : int;
    }
  let make () =
    {count = 0}
end

let request request_type name pattern _ _ timeout _ trans_id pid state api =
  let {ServiceState.count; _} = state in
  let count_new = if count == 4294967295 then 0 else count + 1 in
  state.ServiceState.count <- count_new ;
  print_endline ("count == " ^ (string_of_int count_new) ^ " ocaml") ;
  let response = string_of_int count_new in
  match Cloudi.return_ api
    request_type name pattern "" response timeout trans_id pid with
  | Error (error) ->
    Cloudi.NullError (error)
  | Ok _ ->
    Cloudi.Null

let task thread_index =
  let state = ServiceState.make () in
  match Cloudi.api thread_index state with
  | Error (error) ->
    prerr_endline error
  | Ok (api) ->
    match Cloudi.subscribe api "ocaml/get" request with
    | Error (error) ->
      prerr_endline error
    | Ok _ ->
      match Cloudi.poll api (-1) with
      | Error (error) ->
        prerr_endline error
      | Ok _ ->
        print_endline "terminate count ocaml"

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

