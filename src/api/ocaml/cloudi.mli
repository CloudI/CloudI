(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*

  MIT License

  Copyright (c) 2017-2018 Michael Truog <mjtruog at protonmail dot com>

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

(** OCaml {{:https://cloudi.org/api.html#1_Intro}CloudI API}.
    Example usage is available in the
    {{:https://cloudi.org/tutorials.html#cloudi_examples}integration tests}.
 *)

(** provided when handling a service request *)
type request_type =
    ASYNC
  | SYNC

(** the Erlang pid that is the source of the service request *)
type source = Erlang.Pid.t

(** service request callback function return type *)
type response =
    Response of string
  | ResponseInfo of string * string
  | Forward of string * string * string
  | Forward_ of string * string * string * int * int
  | Null
  | NullError of string

(** an instance of the CloudI API *)
module Instance :
  sig
    type 's t
  end

(** a function to handle a service request *)
type 's callback =
  request_type ->
  string -> string ->
  string -> string ->
  int -> int -> string -> source ->
  's -> 's Instance.t ->
  response

(** a null trans_id is used to check for a timeout or
    to get the oldest response with recv_async *)
val trans_id_null : string

val invalid_input_error : string
val message_decoding_error : string
val terminate_error : string

exception ReturnSync
exception ReturnAsync
exception ForwardSync
exception ForwardAsync

(** creates an instance of the CloudI API *)
val api : int -> 's -> ('s Instance.t, string) result

(** returns the thread count from the service configuration *)
val thread_count : unit -> (int, string) result

(** subscribes to a service name pattern with a callback *)
val subscribe : 's Instance.t -> string -> 's callback -> (unit, string) result

(** returns the number of subscriptions for a single service name pattern *)
val subscribe_count : 's Instance.t -> string -> (int, string) result

(** unsubscribes from a service name pattern once *)
val unsubscribe : 's Instance.t -> string -> (unit, string) result

(** sends an asynchronous service request *)
val send_async :
  ?timeout:int -> ?request_info:string -> ?priority:int ->
  's Instance.t -> string -> string -> (string, string) result

(** sends a synchronous service request *)
val send_sync :
  ?timeout:int -> ?request_info:string -> ?priority:int ->
  's Instance.t -> string -> string -> (string * string * string, string) result

(** sends asynchronous service requests to all subscribers
    of the matching service name pattern *)
val mcast_async :
  ?timeout:int -> ?request_info:string -> ?priority:int ->
  's Instance.t -> string -> string -> (string array, string) result

(** forwards an asynchronous service request to a different service name *)
val forward_async :
  's Instance.t ->
  string -> string -> string ->
  int -> int -> string -> source -> (unit, string) result

(** forwards a synchronous service request to a different service name *)
val forward_sync :
  's Instance.t ->
  string -> string -> string ->
  int -> int -> string -> source -> (unit, string) result

(** forwards a service request to a different service name *)
val forward_ :
  's Instance.t -> request_type ->
  string -> string -> string ->
  int -> int -> string -> source -> (unit, string) result

(** provides a response to an asynchronous service request *)
val return_async :
  's Instance.t ->
  string -> string -> string -> string ->
  int -> string -> source -> (unit, string) result

(** provides a response to a synchronous service request *)
val return_sync :
  's Instance.t ->
  string -> string -> string -> string ->
  int -> string -> source -> (unit, string) result

(** provides a response to a service request *)
val return_ :
  's Instance.t -> request_type ->
  string -> string -> string -> string ->
  int -> string -> source -> (unit, string) result

(** blocks to receive an asynchronous service request response *)
val recv_async :
  ?timeout:int -> ?trans_id:string -> ?consume:bool ->
  's Instance.t -> (string * string * string, string) result

(** returns the 0-based index of this process in the service instance *)
val process_index : 's Instance.t -> int

(** returns the current process count based on the service configuration *)
val process_count : 's Instance.t -> int

(** returns the count_process_dynamic maximum count
    based on the service configuration *)
val process_count_max : 's Instance.t -> int

(** returns the count_process_dynamic minimum count
    based on the service configuration *)
val process_count_min : 's Instance.t -> int

(** returns the service name pattern prefix from the service configuration *)
val prefix : 's Instance.t -> string

(** returns the service initialization timeout
     from the service configuration *)
val timeout_initialize : 's Instance.t -> int

(** returns the default asynchronous service request send timeout
    from the service configuration *)
val timeout_async : 's Instance.t -> int

(** returns the default synchronous service request send timeout
    from the service configuration *)
val timeout_sync : 's Instance.t -> int

(** returns the service termination timeout
    based on the service configuration *)
val timeout_terminate : 's Instance.t -> int

(** blocks to process incoming CloudI service requests *)
val poll : 's Instance.t -> int -> (bool, string) result

(** parses "text_pairs" in service request info *)
val info_key_value_parse : string -> (string, string list) Hashtbl.t

