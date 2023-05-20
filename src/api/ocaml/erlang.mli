(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*

  MIT License

  Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>

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

(** Erlang External Term Format Encoding/Decoding *)

module Pid :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      serial : string;
      creation : string;
    }
  end
module Port :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      creation : string;
    }
  end
module Reference :
  sig
    type t = private {
      node_tag : int;
      node : string;
      id : string;
      creation : string;
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
  | OtpErlangIntegerBig of Big_int_Z.big_int
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

(** Print a binary string *)
val print_binary : string -> unit

(**/**)
(* begin section ignored by ocamldoc *)

(* Unit tests *)
val tests : (string * (unit -> bool)) list

