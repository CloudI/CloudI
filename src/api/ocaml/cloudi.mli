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

type request_type = ASYNC | SYNC
type source = Erlang.Pid.t
type response =
    Null
  | ResponseInfo of string * string
  | Response of string
  | NullError of string
module Instance :
  sig
    type t
  end
type callback =
  Instance.t ->
  request_type ->
  string -> string ->
  string -> string ->
  int -> int -> string -> source -> response

val invalid_input_error : string
val message_decoding_error : string
val terminate_error : string

exception ReturnSync
exception ReturnAsync
exception ForwardSync
exception ForwardAsync

(** creates an instance of the CloudI API *)
val api : int -> (Instance.t, string) result

(** returns the thread count from the service configuration *)
val thread_count : unit -> (int, string) result

(** returns the 0-based index of this process in the service instance *)
val process_index : Instance.t -> int

(** returns the current process count based on the service configuration *)
val process_count : Instance.t -> int

(** returns the count_process_dynamic maximum count
    based on the service configuration *)
val process_count_max : Instance.t -> int

(** returns the count_process_dynamic minimum count
    based on the service configuration *)
val process_count_min : Instance.t -> int

(** returns the service name pattern prefix from the service configuration *)
val prefix : Instance.t -> string

(** returns the service initialization timeout
     from the service configuration *)
val timeout_initialize : Instance.t -> int

(** returns the default asynchronous service request send timeout
    from the service configuration *)
val timeout_async : Instance.t -> int

(** returns the default synchronous service request send timeout
    from the service configuration *)
val timeout_sync : Instance.t -> int

(** returns the service termination timeout
    based on the service configuration *)
val timeout_terminate : Instance.t -> int

val poll : Instance.t -> int -> (bool, string) result

