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

(** parses "text_pairs" from a HTTP GET query string *)
val request_http_qs_parse : string -> (string, string list) Hashtbl.t

(** parses "text_pairs" in service request info *)
val info_key_value_parse : string -> (string, string list) Hashtbl.t

