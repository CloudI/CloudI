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

(**
 * Erlang Binary Term Format Encoding/Decoding
 *)

module Pid :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      serial : string;
      creation : int;
    }
  end
module Port :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  end
module Reference :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  end
module Function :
  sig
    type t = private {
      tag : int;
      value : string;
    }
  end

type t =
    OtpErlangInteger of int
  | OtpErlangIntegerBig of Big_int.big_int
  | OtpErlangFloat of float
  | OtpErlangAtom of string
  | OtpErlangAtomUTF8 of string
  | OtpErlangAtomCacheRef of int
  | OtpErlangAtomBool of bool
  | OtpErlangString of string
  | OtpErlangBinary of string
  | OtpErlangBinaryBits of string * int
  | OtpErlangList of t list
  | OtpErlangListImproper of t list
  | OtpErlangTuple of t list
  | OtpErlangMap of (t, t) Hashtbl.t
  | OtpErlangPid of Pid.t
  | OtpErlangPort of Port.t
  | OtpErlangReference of Reference.t
  | OtpErlangFunction of Function.t

(** Decode Erlang terms within binary data into OCaml types *)
val binary_to_term : string -> (t, string) result

(** Encode OCaml types into Erlang terms in binary data *)
val term_to_binary : t -> (string, string) result

(** Convert the OCaml Erlang type to a string *)
val t_to_string : t -> string

(**/**)
(* begin section ignored by ocamldoc *)

(* Unit tests *)
val tests : (string * (unit -> bool)) list

