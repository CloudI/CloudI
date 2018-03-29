(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*

  MIT License

  Copyright (c) 2017 Michael Truog <mjtruog at protonmail dot com>

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

(* tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html *)
let tag_version = 131
let tag_compressed_zlib = 80
let tag_new_float_ext = 70
let tag_bit_binary_ext = 77
let tag_atom_cache_ref = 78
let tag_small_integer_ext = 97
let tag_integer_ext = 98
let tag_float_ext = 99
let tag_atom_ext = 100
let tag_reference_ext = 101
let tag_port_ext = 102
let tag_pid_ext = 103
let tag_small_tuple_ext = 104
let tag_large_tuple_ext = 105
let tag_nil_ext = 106
let tag_string_ext = 107
let tag_list_ext = 108
let tag_binary_ext = 109
let tag_small_big_ext = 110
let tag_large_big_ext = 111
let tag_new_fun_ext = 112
let tag_export_ext = 113
let tag_new_reference_ext = 114
let tag_small_atom_ext = 115
let tag_map_ext = 116
let tag_fun_ext = 117
let tag_atom_utf8_ext = 118
let tag_small_atom_utf8_ext = 119

let buffer_size = 65536

module Pid = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      serial : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~serial ~creation =
    {node_tag; node; id; serial; creation}
end
module Port = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~creation =
    {node_tag; node; id; creation}
end
module Reference = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~creation =
    {node_tag; node; id; creation}
end
module Function = struct
  type t = {
      tag : int;
      value : string;
    }
  let make ~tag ~value =
    {tag; value}
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

let output_error (s : string) : string =
  "output_error: " ^ s
let parse_error (s : string) : string =
  "parse_error: " ^ s

type ('a,'b,'c) result2 = Ok2 of 'a * 'b | Error2 of 'c
type ('a,'b,'c,'d) result3 = Ok3 of 'a * 'b * 'c | Error3 of 'd

let list_append l1 l2 = List.rev_append (List.rev l1) l2

exception HashtblExit of string

let valid_uint32_positive (value : int) : bool =
  (Int64.of_int value) <= (Int64.of_string "4294967295")

let valid_int32 (value : int) : bool =
  let value64 = Int64.of_int value in
  value64 >= (Int64.of_string "-2147483648") &&
  value64 <= (Int64.of_string "2147483647")

let unpack_uint16 i binary : int =
  let byte0 = int_of_char binary.[i]
  and byte1 = int_of_char binary.[i + 1] in
    ((byte0 lsl 8) lor byte1)

let unpack_uint32 i binary : (int, int, string) result2 =
  let byte0 = int_of_char binary.[i]
  and byte1 = int_of_char binary.[i + 1]
  and byte2 = int_of_char binary.[i + 2]
  and byte3 = int_of_char binary.[i + 3] in
  if byte0 > max_int lsr 24 then
    (* 32 bit system *)
    Error2 (parse_error "ocaml int overflow")
  else
    Ok2 (i + 4,
      (byte0 lsl 24) lor (
        (byte1 lsl 16) lor (
          (byte2 lsl 8) lor byte3
        )
      )
    )

let unpack_integer i binary : t =
  let byte0 = int_of_char binary.[i]
  and byte1 = int_of_char binary.[i + 1]
  and byte2 = int_of_char binary.[i + 2]
  and byte3 = int_of_char binary.[i + 3] in
  let value =
    Int32.logor (
      Int32.shift_left (Int32.of_int byte0) 24
    ) (
      Int32.logor (
        Int32.shift_left (Int32.of_int byte1) 16
      ) (
        Int32.logor (
          Int32.shift_left (Int32.of_int byte2) 8
        ) (
          Int32.of_int byte3
        )
      )
    )
  in
  if byte0 > min_int lsr 24 then
    OtpErlangIntegerBig (Big_int.big_int_of_int32 value)
  else
    OtpErlangInteger (Int32.to_int value)

let unpack_double i binary : float =
  let byte0 = Int64.of_int (int_of_char binary.[i])
  and byte1 = Int64.of_int (int_of_char binary.[i + 1])
  and byte2 = Int64.of_int (int_of_char binary.[i + 2])
  and byte3 = Int64.of_int (int_of_char binary.[i + 3])
  and byte4 = Int64.of_int (int_of_char binary.[i + 4])
  and byte5 = Int64.of_int (int_of_char binary.[i + 5])
  and byte6 = Int64.of_int (int_of_char binary.[i + 6])
  and byte7 = Int64.of_int (int_of_char binary.[i + 7]) in
  Int64.float_of_bits (
    Int64.logor (
      Int64.logor (
        Int64.logor (
          Int64.logor (
            Int64.logor (
              Int64.logor (
                Int64.logor (
                  Int64.shift_left byte0 56
                ) (
                  Int64.shift_left byte1 48
                )
              ) (
                Int64.shift_left byte2 40
              )
            ) (
              Int64.shift_left byte3 32
            )
          ) (
            Int64.shift_left byte4 24
          )
        ) (
          Int64.shift_left byte5 16
        )
      ) (
        Int64.shift_left byte6 8
      )
    ) byte7
  )

let pack_uint16 (value : int) buffer : unit =
  let byte0 = (value asr 8) land 0xff
  and byte1 = value land 0xff in
  Buffer.add_char buffer (char_of_int byte0) ;
  Buffer.add_char buffer (char_of_int byte1)

let pack_uint32 (value : int) buffer : unit =
  let byte0 = (value asr 24) land 0xff
  and byte1 = (value lsr 16) land 0xff
  and byte2 = (value lsr 8) land 0xff
  and byte3 = value land 0xff in
  Buffer.add_char buffer (char_of_int byte0) ;
  Buffer.add_char buffer (char_of_int byte1) ;
  Buffer.add_char buffer (char_of_int byte2) ;
  Buffer.add_char buffer (char_of_int byte3)

let pack_double (value : float) buffer : unit =
  let bits8 = Int64.of_int 0xff
  and value64 = Int64.bits_of_float value in
  let byte0 = Int64.logand (Int64.shift_right_logical value64 56) bits8
  and byte1 = Int64.logand (Int64.shift_right_logical value64 48) bits8
  and byte2 = Int64.logand (Int64.shift_right_logical value64 40) bits8
  and byte3 = Int64.logand (Int64.shift_right_logical value64 32) bits8
  and byte4 = Int64.logand (Int64.shift_right_logical value64 24) bits8
  and byte5 = Int64.logand (Int64.shift_right_logical value64 16) bits8
  and byte6 = Int64.logand (Int64.shift_right_logical value64 8) bits8
  and byte7 = Int64.logand value64 bits8 in
  Buffer.add_char buffer (char_of_int (Int64.to_int byte0)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte1)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte2)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte3)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte4)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte5)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte6)) ;
  Buffer.add_char buffer (char_of_int (Int64.to_int byte7))

let rec binary_to_term_ i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_new_float_ext then
    Ok2 (i0 + 8, OtpErlangFloat (unpack_double i0 binary))
  else if tag = tag_bit_binary_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      let bits = int_of_char binary.[i1]
      and i2 = i1 + 1 in
      let data = String.sub binary i2 j in
      let term : t = if bits = 8 then
        OtpErlangBinary (data)
      else
        OtpErlangBinaryBits ((data, bits))
      in
      Ok2(i2 + j, term)
  else if tag = tag_atom_cache_ref then
    Ok2 (i0 + 1, OtpErlangAtomCacheRef (int_of_char binary.[i0]))
  else if tag = tag_small_integer_ext then
    Ok2 (i0 + 1, OtpErlangInteger (int_of_char binary.[i0]))
  else if tag = tag_integer_ext then
    Ok2 (i0 + 4, unpack_integer i0 binary)
  else if tag = tag_float_ext then
    let data = String.sub binary i0 31 in
    let float_data = try (String.sub data 0 (String.index data '\000'))
    with Not_found -> data in
    Ok2 (i0 + 31, OtpErlangFloat (float_of_string float_data))
  else if tag = tag_atom_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangAtom (String.sub binary i1 j))
  else if tag = tag_reference_ext || tag = tag_port_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let creation = int_of_char binary.[i2]
      and i3 = i2 + 1 in
      if tag = tag_reference_ext then
        Ok2 (i3, OtpErlangReference (
          Reference.make
            ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
      else (* tag = tag_port_ext *)
        Ok2 (i3, OtpErlangPort (
          Port.make
            ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
  else if tag = tag_pid_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let serial = String.sub binary i2 4
      and i3 = i2 + 4 in
      let creation = int_of_char binary.[i3]
      and i4 = i3 + 1 in
      Ok2 (i4, OtpErlangPid (
        Pid.make
          ~node_tag:node_tag ~node:node
          ~id:id ~serial:serial ~creation:creation))
  else if tag = tag_small_tuple_ext then
    let length = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    match binary_to_term_sequence i1 length binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i2, tmp) ->
      Ok2 (i2, OtpErlangTuple (tmp))
  else if tag = tag_large_tuple_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (_, length) when length > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, length) ->
      match binary_to_term_sequence i1 length binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, tmp) ->
        Ok2 (i2, OtpErlangTuple (tmp))
  else if tag = tag_nil_ext then
    Ok2 (i0, OtpErlangList ([]))
  else if tag = tag_string_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangString (String.sub binary i1 j))
  else if tag = tag_list_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (i1, length) ->
      match binary_to_term_sequence i1 length binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, tmp) ->
        match binary_to_term_ i2 binary with
        | Error2 (error) ->
          Error2 (error)
        | Ok2 (i3, tail) when tail = OtpErlangList([]) ->
          Ok2 (i3, OtpErlangList (tmp))
        | Ok2 (i3, tail) ->
          Ok2 (i3, OtpErlangListImproper (list_append tmp [tail]))
  else if tag = tag_binary_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      Ok2 (i1 + j, OtpErlangBinary (String.sub binary i1 j))
  else if tag = tag_small_big_ext || tag = tag_large_big_ext then
    let length_f () =
      if tag = tag_small_big_ext then
        Ok2 (i0 + 1, int_of_char binary.[i0])
      else (* tag = tag_large_big_ext *)
        unpack_uint32 i0 binary
    in
    match length_f () with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      let sign = int_of_char binary.[i1] in
      let rec loop bignum_index bignum_value =
        if bignum_index = j then
          bignum_value
        else
          loop
            (bignum_index + 1)
            (Big_int.add_int_big_int
              (int_of_char binary.[i1 + j - bignum_index])
              (Big_int.mult_int_big_int 256 bignum_value))
      in
      let bignum = loop 0 Big_int.zero_big_int in
      let bignum_result = if sign = 1 then
        Big_int.minus_big_int bignum
      else
        bignum
      and i2 = i1 + 1 in
      Ok2 (i2 + j, OtpErlangIntegerBig (bignum_result))
  else if tag = tag_new_fun_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, length) ->
      let value = String.sub binary i1 length in
      Ok2 (i1 + length, OtpErlangFunction (
        Function.make
          ~tag:tag ~value:value))
  else if tag = tag_export_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, _, _) ->
      match binary_to_atom i1 binary with
      | Error3 (error) ->
        Error2 (error)
      | Ok3 (i2, _, _) when int_of_char binary.[i2] <> tag_small_integer_ext ->
        Error2 (parse_error "invalid small integer tag")
      | Ok3 (i2, _, _) ->
        let i3 = i2 + 2 in
        let value = String.sub binary i0 (i3 - i0) in
        Ok2 (i3, OtpErlangFunction (
          Function.make
            ~tag:tag ~value:value))
  else if tag = tag_new_reference_ext then
    let j = (unpack_uint16 i0 binary) * 4
    and i1 = i0 + 2 in
    match binary_to_atom i1 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3 (i2, node_tag, node) ->
      let creation = int_of_char binary.[i2]
      and i3 = i2 + 1 in
      let id = String.sub binary i3 j in
      Ok2 (i3 + j, OtpErlangReference(
        Reference.make
          ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
  else if tag = tag_small_atom_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    let atom_name = String.sub binary i1 j in
    if atom_name = "true" then
      Ok2 (i1 + j, OtpErlangAtomBool (true))
    else if atom_name = "false" then
      Ok2 (i1 + j, OtpErlangAtomBool (false))
    else
      Ok2 (i1 + j, OtpErlangAtom (atom_name))
  else if tag = tag_map_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, length) ->
      let pairs = Hashtbl.create length in
      let rec loop length_index i2 =
        if length_index = length then
          Ok2 (i2, OtpErlangMap (pairs))
        else
          match binary_to_term_ i2 binary with
          | Error2 (error) ->
            Error2 (error)
          | Ok2 (i3, key) ->
            match binary_to_term_ i3 binary with
            | Error2 (error) ->
              Error2 (error)
            | Ok2 (i4, value) ->
              Hashtbl.add pairs key value ;
              loop (length_index + 1) i4
      in
      loop 0 i1
  else if tag = tag_fun_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, numfree) ->
      match binary_to_pid i1 binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, _) -> (* pid *)
        match binary_to_atom i2 binary with
        | Error3 (error) ->
          Error2 (error)
        | Ok3 (i3, _, _) -> (* module *)
          match binary_to_integer i3 binary with
          | Error2 (error) ->
            Error2 (error)
          | Ok2 (i4, _) -> (* index *)
            match binary_to_integer i4 binary with
            | Error2 (error) ->
              Error2 (error)
            | Ok2 (i5, _) -> (* uniq *)
              match binary_to_term_sequence i5 numfree binary with
              | Error2 (error) ->
                Error2 (error)
              | Ok2 (i6, _) -> (* free *)
                let value = String.sub binary i0 (i6 - i0) in
                Ok2 (i6, OtpErlangFunction (
                  Function.make
                    ~tag:tag ~value:value))
  else if tag = tag_atom_utf8_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangAtomUTF8 (String.sub binary i1 j))
  else if tag = tag_small_atom_utf8_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    Ok2 (i1 + j, OtpErlangAtomUTF8 (String.sub binary i1 j))
  else if tag = tag_compressed_zlib then
    Error2 (parse_error "ocaml doesn't provide zlib")
  else
    Error2 (parse_error "invalid tag")

and binary_to_term_sequence i length binary : (int, t list, string) result2 =
  let rec loop length_index i0 sequence =
    if length_index = length then
      Ok2 (i0, List.rev sequence)
    else
      match binary_to_term_ i0 binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i1, element) ->
        loop (length_index + 1) i1 ([element] @ sequence)
  in
  loop 0 i []

and binary_to_integer i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_small_integer_ext then
    Ok2 (i0 + 1, OtpErlangInteger (int_of_char binary.[i0]))
  else if tag = tag_integer_ext then
    Ok2 (i0 + 4, unpack_integer i0 binary)
  else
    Error2 (parse_error "invalid integer tag")

and binary_to_pid i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_pid_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let serial = String.sub binary i2 4
      and i3 = i2 + 4 in
      let creation = int_of_char binary.[i3]
      and i4 = i3 + 1 in
      Ok2 (i4, OtpErlangPid (
        Pid.make
          ~node_tag:node_tag ~node:node
          ~id:id ~serial:serial ~creation:creation))
  else
    Error2 (parse_error "invalid pid tag")

and binary_to_atom i binary : (int, int, string, string) result3 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_atom_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok3 (i1 + j, tag, String.sub binary i0 (2 + j))
  else if tag = tag_atom_cache_ref then
    Ok3 (i0 + 1, tag, String.make 1 binary.[i0])
  else if tag = tag_small_atom_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    Ok3 (i1 + j, tag, String.sub binary i0 (1 + j))
  else if tag = tag_atom_utf8_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok3 (i1 + j, tag, String.sub binary i0 (2 + j))
  else if tag = tag_small_atom_utf8_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    Ok3 (i1 + j, tag, String.sub binary i0 (1 + j))
  else
    Error3 (parse_error "invalid atom tag")

let rec term_to_binary_ term buffer : (Buffer.t, string) result =
  match term with
  | OtpErlangInteger (value) ->
    integer_to_binary value buffer
  | OtpErlangIntegerBig (value) ->
    bignum_to_binary value buffer
  | OtpErlangFloat (value) ->
    float_to_binary value buffer
  | OtpErlangAtom (value) ->
    atom_to_binary value buffer
  | OtpErlangAtomUTF8 (value) ->
    atom_utf8_to_binary value buffer
  | OtpErlangAtomCacheRef (value) ->
    Buffer.add_char buffer (char_of_int tag_atom_cache_ref) ;
    Buffer.add_char buffer (char_of_int value) ;
    Ok (buffer)
  | OtpErlangAtomBool (value) ->
    if value then
      atom_to_binary "true" buffer
    else
      atom_to_binary "false" buffer
  | OtpErlangString (value) ->
    string_to_binary value buffer
  | OtpErlangBinary (value) ->
    binary_bits_to_binary value 8 buffer
  | OtpErlangBinaryBits (value, bits) ->
    binary_bits_to_binary value bits buffer
  | OtpErlangList (value) ->
    list_to_binary value false buffer
  | OtpErlangListImproper (value) ->
    list_to_binary value true buffer
  | OtpErlangTuple (value) ->
    tuple_to_binary value buffer
  | OtpErlangMap (value) ->
    hashtbl_to_binary value buffer
  | OtpErlangPid ({Pid.node_tag; node; id; serial; creation}) ->
    pid_to_binary node_tag node id serial creation buffer
  | OtpErlangPort ({Port.node_tag; node; id; creation}) ->
    port_to_binary node_tag node id creation buffer
  | OtpErlangReference ({Reference.node_tag; node; id; creation}) ->
    reference_to_binary node_tag node id creation buffer
  | OtpErlangFunction ({Function.tag; value}) ->
    function_to_binary tag value buffer

and string_to_binary value buffer =
  let length = String.length value in
  if length = 0 then (
    Buffer.add_char buffer (char_of_int tag_nil_ext) ;
    Ok (buffer))
  else if length <= 65535 then (
    Buffer.add_char buffer (char_of_int tag_string_ext) ;
    pack_uint16 length buffer ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else if valid_uint32_positive length then (
    Buffer.add_char buffer (char_of_int tag_list_ext) ;
    pack_uint32 length buffer ;
    String.iter (fun c ->
      Buffer.add_char buffer (char_of_int tag_small_integer_ext) ;
      Buffer.add_char buffer c
    ) value ;
    Buffer.add_char buffer (char_of_int tag_nil_ext) ;
    Ok (buffer))
  else
    Error (output_error "uint32 overflow")

and tuple_to_binary value buffer =
  let rec loop = function
    | [] ->
      Ok (buffer)
    | h::t ->
      match term_to_binary_ h buffer with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        loop t
  and length = List.length value in
  if length <= 255 then (
    Buffer.add_char buffer (char_of_int tag_small_tuple_ext) ;
    Buffer.add_char buffer (char_of_int length) ;
    loop value)
  else if valid_uint32_positive length then (
    Buffer.add_char buffer (char_of_int tag_large_tuple_ext) ;
    pack_uint32 length buffer ;
    loop value)
  else
    Error (output_error "uint32 overflow")

and hashtbl_to_binary value buffer =
  let length = Hashtbl.length value in
  if valid_uint32_positive length then (
    pack_uint32 length buffer ;
    try (Hashtbl.iter (fun key value ->
      match term_to_binary_ key buffer with
      | Error (error) ->
        raise (HashtblExit error)
      | Ok _ ->
        match term_to_binary_ value buffer with
        | Error (error) ->
          raise (HashtblExit error)
        | Ok _ ->
          ()
    ) value ; Ok (buffer))
    with
      HashtblExit (error) ->
        Error (error))
  else
    Error (output_error "uint32 overflow")

and list_to_binary value improper buffer =
  let rec loop = function
    | [] ->
      Ok (buffer)
    | h::t ->
      match term_to_binary_ h buffer with
      | Ok _ ->
        loop t
      | Error (error) ->
        Error (error)
  and length = List.length value in
  if length = 0 then (
    Buffer.add_char buffer (char_of_int tag_nil_ext) ;
    Ok (buffer))
  else if valid_uint32_positive length then (
    Buffer.add_char buffer (char_of_int tag_list_ext) ; (
    if improper then
      pack_uint32 (length - 1) buffer
    else
      pack_uint32 length buffer) ;
    match loop value with
    | Error (error) ->
      Error (error)
    | Ok (_) when not improper ->
      Buffer.add_char buffer (char_of_int tag_nil_ext) ;
      Ok (buffer)
    | Ok (_) ->
      Ok (buffer))
  else
    Error (output_error "uint32 overflow")

and integer_to_binary value buffer =
  if value >= 0 && value <= 255 then (
    Buffer.add_char buffer (char_of_int tag_small_integer_ext) ;
    Buffer.add_char buffer (char_of_int value) ;
    Ok (buffer))
  else if valid_int32 value then (
    Buffer.add_char buffer (char_of_int tag_integer_ext) ;
    pack_uint32 value buffer ;
    Ok (buffer))
  else
    bignum_to_binary (Big_int.big_int_of_int value) buffer

and bignum_to_binary value buffer =
  let bits8 = Big_int.big_int_of_int 0xff
  and sign = if (Big_int.sign_big_int value) = -1 then
    char_of_int 1
  else
    char_of_int 0
  in
  let rec loop bignum l =
    if Big_int.gt_big_int bignum Big_int.zero_big_int then (
      Buffer.add_char l (char_of_int
        (Big_int.int_of_big_int (Big_int.and_big_int bignum bits8))) ;
      loop (Big_int.shift_right_towards_zero_big_int bignum 8) l)
    else
      l
  in
  let l_result = loop (Big_int.abs_big_int value) (Buffer.create 255) in
  let l_length = Buffer.length l_result in
  if l_length <= 255 then (
    Buffer.add_char buffer (char_of_int tag_small_big_ext) ;
    Buffer.add_char buffer (char_of_int l_length) ;
    Buffer.add_char buffer sign ;
    Buffer.add_buffer buffer l_result ;
    Ok (buffer))
  else if (valid_uint32_positive l_length) then (
    Buffer.add_char buffer (char_of_int tag_large_big_ext) ;
    pack_uint32 l_length buffer ;
    Buffer.add_char buffer sign ;
    Buffer.add_buffer buffer l_result ;
    Ok (buffer))
  else
    Error (output_error "uint32 overflow")

and float_to_binary value buffer =
  Buffer.add_char buffer (char_of_int tag_new_float_ext) ;
  pack_double value buffer ;
  Ok (buffer)

and atom_to_binary value buffer =
  let length = String.length value in
  if length <= 255 then (
    Buffer.add_char buffer (char_of_int tag_small_atom_ext) ;
    Buffer.add_char buffer (char_of_int length) ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else if length <= 65535 then (
    Buffer.add_char buffer (char_of_int tag_atom_ext) ;
    pack_uint16 length buffer ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else
    Error (output_error "uint16 overflow")

and atom_utf8_to_binary value buffer =
  let length = String.length value in
  if length <= 255 then (
    Buffer.add_char buffer (char_of_int tag_small_atom_utf8_ext) ;
    Buffer.add_char buffer (char_of_int length) ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else if length <= 65535 then (
    Buffer.add_char buffer (char_of_int tag_atom_utf8_ext) ;
    pack_uint16 length buffer ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else
    Error (output_error "uint16 overflow")

and binary_bits_to_binary value bits buffer =
  let length = String.length value in
  if bits < 1 || bits > 8 then
    Error (output_error "invalid OtpErlangBinaryBits")
  else if valid_uint32_positive length then (
    if bits <> 8 then (
      Buffer.add_char buffer (char_of_int tag_bit_binary_ext) ;
      pack_uint32 length buffer ;
      Buffer.add_char buffer (char_of_int bits))
    else (
      Buffer.add_char buffer (char_of_int tag_binary_ext) ;
      pack_uint32 length buffer) ;
    Buffer.add_string buffer value ;
    Ok (buffer))
  else
    Error (output_error "uint32 overflow")

and function_to_binary tag value buffer =
  Buffer.add_char buffer (char_of_int tag) ;
  Buffer.add_string buffer value ;
  Ok (buffer)

and pid_to_binary node_tag node id serial creation buffer =
  Buffer.add_char buffer (char_of_int tag_pid_ext) ;
  Buffer.add_char buffer (char_of_int node_tag) ;
  Buffer.add_string buffer node ;
  Buffer.add_string buffer id ;
  Buffer.add_string buffer serial ;
  Buffer.add_char buffer (char_of_int creation) ;
  Ok (buffer)

and port_to_binary node_tag node id creation buffer =
  Buffer.add_char buffer (char_of_int tag_port_ext) ;
  Buffer.add_char buffer (char_of_int node_tag) ;
  Buffer.add_string buffer node ;
  Buffer.add_string buffer id ;
  Buffer.add_char buffer (char_of_int creation) ;
  Ok (buffer)

and reference_to_binary node_tag node id creation buffer =
  let length = (String.length id) / 4 in
  if length = 0 then (
    Buffer.add_char buffer (char_of_int tag_reference_ext) ;
    Buffer.add_char buffer (char_of_int node_tag) ;
    Buffer.add_string buffer node ;
    Buffer.add_string buffer id ;
    Buffer.add_char buffer (char_of_int creation) ;
    Ok (buffer))
  else if length <= 65535 then (
    Buffer.add_char buffer (char_of_int tag_new_reference_ext) ;
    pack_uint16 length buffer ;
    Buffer.add_char buffer (char_of_int node_tag) ;
    Buffer.add_string buffer node ;
    Buffer.add_char buffer (char_of_int creation) ;
    Buffer.add_string buffer id ;
    Ok (buffer))
  else
    Error (output_error "uint16 overflow")

let binary_to_term (binary : string) : (t, string) result =
  let size = String.length binary in
  if size <= 1 then
    Error (parse_error "null input")
  else if int_of_char binary.[0] <> tag_version then
    Error (parse_error "invalid version")
  else
    try
      match binary_to_term_ 1 binary with
      | Error2 error ->
        Error error
      | Ok2 (i, _) when i <> size ->
        Error (parse_error "unparsed data")
      | Ok2 (_, term) ->
        Ok term
    with
      Invalid_argument(_) ->
        Error (parse_error "missing data")

let term_to_binary (term : t) : (string, string) result =
  let buffer = Buffer.create buffer_size in
  Buffer.add_char buffer (char_of_int tag_version) ;
  match term_to_binary_ term buffer with
  | Ok (data_uncompressed) ->
    Ok (Buffer.contents data_uncompressed)
  | Error (error) ->
    Error (error)

let rec t_to_string (term : t) : string =
  match term with
  | OtpErlangInteger (value) ->
    "OtpErlangInteger(" ^ (string_of_int value) ^ ")"
  | OtpErlangIntegerBig (value) ->
    "OtpErlangIntegerBig(" ^ (Big_int.string_of_big_int value) ^ ")"
  | OtpErlangFloat (value) ->
    "OtpErlangFloat(" ^ (string_of_float value) ^ ")"
  | OtpErlangAtom (value) ->
    "OtpErlangAtom('" ^ value ^ "')"
  | OtpErlangAtomUTF8 (value) ->
    "OtpErlangAtomUTF8('" ^ value ^ "')"
  | OtpErlangAtomCacheRef (value) ->
    "OtpErlangAtomUTF8('atom(" ^ (string_of_int value) ^ ")')"
  | OtpErlangAtomBool (value) ->
    if value then
      "OtpErlangAtomBool('true')"
    else
      "OtpErlangAtomBool('false')"
  | OtpErlangString (value) ->
    "OtpErlangString(\"" ^ value ^ "\")"
  | OtpErlangBinary (value) ->
    "OtpErlangBinary(\"" ^ value ^ "\")"
  | OtpErlangBinaryBits (value, bits) ->
    "OtpErlangBinaryBits(\"" ^ value ^ "\"," ^ (string_of_int bits) ^ ")"
  | OtpErlangList (value) ->
    "OtpErlangList(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangListImproper (value) ->
    "OtpErlangListImproper(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangTuple (value) ->
    "OtpErlangTuple(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangMap (value) ->
    "OtpErlangMap(" ^ (map_to_string value) ^ ")"
  | OtpErlangPid ({Pid.node_tag; node; id; serial; creation}) ->
    "OtpErlangPid(" ^
      "node_tag:" ^ (string_of_int node_tag) ^ "; " ^
      "node:\"" ^ (String.escaped node) ^ "\"; " ^
      "id:\"" ^ (String.escaped id) ^ "\"; " ^
      "serial:\"" ^ (String.escaped serial) ^ "\"; " ^
      "creation:" ^ (string_of_int creation) ^ ")"
  | OtpErlangPort ({Port.node_tag; node; id; creation}) ->
    "OtpErlangPort(" ^
      "node_tag:" ^ (string_of_int node_tag) ^ "; " ^
      "node:\"" ^ (String.escaped node) ^ "\"; " ^
      "id:\"" ^ (String.escaped id) ^ "\"; " ^
      "creation:" ^ (string_of_int creation) ^ ")"
  | OtpErlangReference ({Reference.node_tag; node; id; creation}) ->
    "OtpErlangReference(" ^
      "node_tag:" ^ (string_of_int node_tag) ^ "; " ^
      "node:\"" ^ (String.escaped node) ^ "\"; " ^
      "id:\"" ^ (String.escaped id) ^ "\"; " ^
      "creation:" ^ (string_of_int creation) ^ ")"
  | OtpErlangFunction ({Function.tag; value}) ->
    "OtpErlangFunction(" ^
      "tag:" ^ (string_of_int tag) ^ "; " ^
      "value:\"" ^ (String.escaped value) ^ ")"

and sequence_to_string (terms : t list) : string =
  let buffer = Buffer.create (32 * (List.length terms)) in
  List.iter (fun term ->
    if (Buffer.length buffer) = 0 then
      Buffer.add_string buffer "["
    else
      Buffer.add_string buffer "," ;
    Buffer.add_string buffer (t_to_string term)) terms ;
  Buffer.add_string buffer "]" ;
  Buffer.contents buffer

and map_to_string (terms : (t, t) Hashtbl.t) : string =
  let buffer = Buffer.create (32 * (Hashtbl.length terms)) in
  Hashtbl.iter (fun key value ->
    if (Buffer.length buffer) = 0 then
      Buffer.add_string buffer "{"
    else
      Buffer.add_string buffer "," ;
    Buffer.add_string buffer (t_to_string key) ;
    Buffer.add_string buffer " => " ;
    Buffer.add_string buffer (t_to_string value) ;
    Buffer.add_string buffer ",") terms ;
  Buffer.add_string buffer "}" ;
  Buffer.contents buffer

exception TermOk of string
let term_ok (value : (t, string) result) : t =
  match value with
  | Ok (term) ->
    term
  | Error (error) ->
    raise (TermOk error)

exception TermError of string
let term_error (value : (t, string) result) : string =
  match value with
  | Ok (term) ->
    raise (TermError (t_to_string term))
  | Error (error) ->
    error

exception BinaryOk of string
let binary_ok (value : (string, string) result) : string =
  match value with
  | Ok (binary) ->
    binary
  | Error (error) ->
    raise (BinaryOk error)

let register_printers () =
  Printexc.register_printer (function
    | TermOk e ->
      Some ("term_ok " ^ e)
    | TermError e ->
      Some ("term_error " ^ e)
    | BinaryOk e ->
      Some ("binary_ok " ^ e)
    | _ ->
      None
  )

(*

  MIT LICENSE (of tests below)

  Copyright (c) 2017 Michael Truog <mjtruog at protonmail dot com>
  Copyright (c) 2009-2013 Dmitry Vasiliev <dima@hlabs.org>

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

let rec string_create_impl size s b =
  if size = 0 then
    Buffer.contents b
  else (
    Buffer.add_string b s ;
    string_create_impl (size - 1) s b)
and string_create size s =
  string_create_impl size s (Buffer.create (size * (String.length s)))

let test_pid () =
  let pid1 = OtpErlangPid (Pid.make
    ~node_tag:100
    ~node:"\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e\x6f\x68\x6f\x73\x74"
    ~id:"\x00\x00\x00\x3b" ~serial:"\x00\x00\x00\x00" ~creation:0)
  and binary1 = "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F" ^
    "\x68\x6F\x73\x74\x00\x00\x00\x3B\x00\x00\x00\x00\x00"
  and pid2 = OtpErlangPid (Pid.make
    ~node_tag:119
    ~node:"\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74"
    ~id:"\x00\x00\x00\x50" ~serial:"\x00\x00\x00\x00" ~creation:0)
  and binary2 = "\x83\x67\x77\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68" ^
    "\x6F\x73\x74\x00\x00\x00\x50\x00\x00\x00\x00\x00" in
  assert ((binary_ok (term_to_binary pid1)) = binary1) ;
  assert ((term_ok (binary_to_term binary1)) = pid1) ;
  assert ((binary_ok (term_to_binary pid2)) = binary2) ;
  assert ((term_ok (binary_to_term binary2)) = pid2) ;
  true

let test_function () =
  let function1 = OtpErlangFunction (Function.make
    ~tag:113
    ~value:(
      "\x64\x00\x05\x6c\x69\x73\x74\x73\x64" ^
      "\x00\x06\x6d\x65\x6d\x62\x65\x72\x61\x02")
    )
  and binary = "\x83\x71\x64\x00\x05\x6C\x69\x73\x74\x73\x64\x00\x06\x6D" ^
    "\x65\x6D\x62\x65\x72\x61\x02" in
  assert ((binary_ok (term_to_binary function1)) = binary) ;
  assert ((term_ok (binary_to_term binary)) = function1) ;
  true

let test_reference () =
  let reference1 = OtpErlangReference (Reference.make
    ~node_tag:100
    ~node:"\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e\x6f\x68\x6f\x73\x74"
    ~id:"\x00\x00\x00\xaf\x00\x00\x00\x03\x00\x00\x00\x00" ~creation:0)
  and binary = "\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40" ^
    "\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\xAF\x00\x00\x00\x03\x00\x00" ^
    "\x00\x00" in
  assert ((binary_ok (term_to_binary reference1)) = binary) ;
  assert ((term_ok (binary_to_term binary)) = reference1) ;
  true

let test_decode_basic () =
  assert (
    (term_error (binary_to_term "")) =
    "parse_error: null input") ;
  assert (
    (term_error (binary_to_term "\x83")) =
    "parse_error: null input") ;
  assert (
    (term_error (binary_to_term "\x83z")) =
    "parse_error: invalid tag") ;
  true

let test_decode_atom () =
  assert (
    (term_error (binary_to_term "\x83d")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83d\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83d\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83d\x00\x00")) =
    OtpErlangAtom ("")) ;
  assert (
    (term_ok (binary_to_term "\x83s\x00")) =
    OtpErlangAtom ("")) ;
  assert (
    (term_ok (binary_to_term "\x83d\x00\x04test")) =
    OtpErlangAtom ("test")) ;
  assert (
    (term_ok (binary_to_term "\x83s\x04test")) =
    OtpErlangAtom ("test")) ;
  true

let test_decode_predefined_atom () =
  assert (
    (term_ok (binary_to_term "\x83s\x04true")) =
    OtpErlangAtomBool (true)) ;
  assert (
    (term_ok (binary_to_term "\x83s\x05false")) =
    OtpErlangAtomBool (false)) ;
  assert (
    (term_ok (binary_to_term "\x83s\x09undefined")) =
    OtpErlangAtom ("undefined")) ;
  true

let test_decode_empty_list () =
  assert (
    (term_ok (binary_to_term "\x83j")) =
    OtpErlangList ([])) ;
  true

let test_decode_string_list () =
  assert (
    (term_error (binary_to_term "\x83k")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83k\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83k\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83k\x00\x00")) =
    OtpErlangString ("")) ;
  assert (
    (term_ok (binary_to_term "\x83k\x00\x04test")) =
    OtpErlangString ("test")) ;
  true

let test_decode_list () =
  assert (
    (term_error (binary_to_term "\x83l")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83l\x00\x00\x00\x00j")) =
    OtpErlangList ([])) ;
  assert (
    (term_ok (binary_to_term "\x83l\x00\x00\x00\x02jjj")) =
    OtpErlangList ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_improper_list () =
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00\x00k")) =
    "parse_error: missing data") ;
  let lst = term_ok (binary_to_term "\x83l\x00\x00\x00\x01jd\x00\x04tail") in
  assert (
    lst =
    OtpErlangListImproper ([OtpErlangList ([]); OtpErlangAtom ("tail")])) ;
  true

let test_decode_small_tuple () =
  assert (
    (term_error (binary_to_term "\x83h")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83h\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83h\x00")) =
    OtpErlangTuple ([])) ;
  assert (
    (term_ok (binary_to_term "\x83h\x02jj")) =
    OtpErlangTuple ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_large_tuple () =
  assert (
    (term_error (binary_to_term "\x83i")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83i\x00\x00\x00\x00")) =
    OtpErlangTuple ([])) ;
  assert (
    (term_ok (binary_to_term "\x83i\x00\x00\x00\x02jj")) =
    OtpErlangTuple ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_small_integer () =
  assert (
    (term_error (binary_to_term "\x83a")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83a\x00")) =
    OtpErlangInteger (0)) ;
  assert (
    (term_ok (binary_to_term "\x83a\xff")) =
    OtpErlangInteger (255)) ;
  true

let test_decode_integer () =
  assert (
    (term_error (binary_to_term "\x83b")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83b\x00\x00\x00\x00")) =
    OtpErlangInteger (0)) ;
  assert (
    (term_ok (binary_to_term "\x83b\x7f\xff\xff\xff")) =
    OtpErlangInteger (Int32.to_int Int32.max_int)) ;
  assert (
    (term_ok (binary_to_term "\x83b\x80\x00\x00\x00")) =
    OtpErlangInteger (Int32.to_int Int32.min_int)) ;
  assert (
    (term_ok (binary_to_term "\x83b\xff\xff\xff\xff")) =
    OtpErlangInteger (-1)) ;
  let max_int_plus_1 = OtpErlangIntegerBig (
    Big_int.big_int_of_string "4611686018427387904")
  and max_int_plus_1_check = term_ok
    (binary_to_term "\x83n\x08\x00\x00\x00\x00\x00\x00\x00\x00@") in
  assert (
    (t_to_string max_int_plus_1) =
    (t_to_string max_int_plus_1_check)) ;
  true

let test_decode_binary () =
  assert (
    (term_error (binary_to_term "\x83m")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83m\x00\x00\x00\x00")) =
    OtpErlangBinary ("")) ;
  assert (
    (term_ok (binary_to_term "\x83m\x00\x00\x00\x04data")) =
    OtpErlangBinary ("data")) ;
  true

let test_decode_float () =
  assert (
    (term_error (binary_to_term "\x83F")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00\x00\x00")) =
    OtpErlangFloat (0.0)) ;
  assert (
    (term_ok (binary_to_term "\x83F?\xf8\x00\x00\x00\x00\x00\x00")) =
    OtpErlangFloat (1.5)) ;
  true

let test_decode_small_big_integer () =
  assert (
    (term_error (binary_to_term "\x83n")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83n\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83n\x01\x00")) =
    "parse_error: missing data") ;
  let zero = OtpErlangIntegerBig (Big_int.zero_big_int)
  and zero_check = term_ok
    (binary_to_term "\x83n\x00\x00") in
  assert (
    (t_to_string zero) =
    (t_to_string zero_check)) ;
  let bigint1 = OtpErlangIntegerBig (
    Big_int.big_int_of_string "6618611909121")
  and bigint1_check = term_ok
    (binary_to_term "\x83n\x06\x00\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint1) =
    (t_to_string bigint1_check)) ;
  let bigint2 = OtpErlangIntegerBig (
    Big_int.big_int_of_string "-6618611909121")
  and bigint2_check = term_ok
    (binary_to_term "\x83n\x06\x01\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint2) =
    (t_to_string bigint2_check)) ;
  true

let test_decode_large_big_integer () =
  assert (
    (term_error (binary_to_term "\x83o")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00\x01\x00")) =
    "parse_error: missing data") ;
  let zero = OtpErlangIntegerBig (Big_int.zero_big_int)
  and zero_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x00\x00") in
  assert (
    (t_to_string zero) =
    (t_to_string zero_check)) ;
  let bigint1 = OtpErlangIntegerBig (
    Big_int.big_int_of_string "6618611909121")
  and bigint1_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x06\x00\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint1) =
    (t_to_string bigint1_check)) ;
  let bigint2 = OtpErlangIntegerBig (
    Big_int.big_int_of_string "-6618611909121")
  and bigint2_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x06\x01\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint2) =
    (t_to_string bigint2_check)) ;
  true

let test_encode_tuple () =
  assert (
    (binary_ok (term_to_binary (OtpErlangTuple ([])))) =
    "\x83h\x00") ;
  let tuple1 = OtpErlangTuple ([OtpErlangTuple ([]); OtpErlangTuple ([])]) in
  assert (
    (binary_ok (term_to_binary tuple1)) =
    "\x83h\x02h\x00h\x00") ;
  let rec tuple_create size l =
    if size = 0 then
      OtpErlangTuple (l)
    else
      tuple_create (size - 1) ([OtpErlangTuple ([])] @ l) in
  assert (
    (binary_ok (term_to_binary (tuple_create 255 []))) =
    "\x83h\xff" ^ (string_create 255 "h\x00")) ;
  assert (
    (binary_ok (term_to_binary (tuple_create 256 []))) =
    "\x83i\x00\x00\x01\x00" ^ (string_create 256 "h\x00")) ;
  true

let test_encode_empty_list () =
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([])))) =
    "\x83j") ;
  true

let test_encode_string_list () =
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("")))) =
    "\x83j") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("\x00")))) =
    "\x83k\x00\x01\x00") ;
  let s = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r" ^
          "\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a" ^
          "\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>" ^
          "?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopq" ^
          "rstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88" ^
          "\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95" ^
          "\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2" ^
          "\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf" ^
          "\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc" ^
          "\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9" ^
          "\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6" ^
          "\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3" ^
          "\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0" ^
          "\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff" in
  assert (
    (binary_ok (term_to_binary (OtpErlangString (s)))) =
    "\x83k\x01\x00" ^ s) ;
  true

let test_encode_list_basic () =
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("")))) =
    "\x83\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangString("")])))) =
    "\x83\x6C\x00\x00\x00\x01\x6A\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (1)])))) =
    "\x83\x6C\x00\x00\x00\x01\x61\x01\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (255)])))) =
    "\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (256)])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A") ;
  let integer1 = OtpErlangInteger (Int32.to_int Int32.max_int) in
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([integer1])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A") ;
  let bigint1 = OtpErlangIntegerBig (
    Big_int.add_int_big_int 1 (Big_int.big_int_of_int32 Int32.max_int)) in
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([bigint1])))) =
    "\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (0)])))) =
    "\x83\x6C\x00\x00\x00\x01\x61\x00\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (-1)])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (-256)])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangInteger (-257)])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A") ;
  let integer2 = OtpErlangInteger (Int32.to_int Int32.min_int) in
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([integer2])))) =
    "\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A") ;
  let bigint2 = OtpErlangIntegerBig (
    Big_int.add_int_big_int (-1) (Big_int.big_int_of_int32 Int32.min_int)) in
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([bigint2])))) =
    "\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangString ("test")])))) =
    "\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A") ;
  let list1 = OtpErlangList ([
    OtpErlangInteger (373); OtpErlangInteger (455)]) in
  assert (
    (binary_ok (term_to_binary list1)) =
    "\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00\x01\xC7\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangList ([])])))) =
    "\x83\x6C\x00\x00\x00\x01\x6A\x6A") ;
  let list2 = OtpErlangList ([OtpErlangList ([]); OtpErlangList ([])]) in
  assert (
    (binary_ok (term_to_binary list2)) =
    "\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A") ;
  let list3 = OtpErlangList ([OtpErlangList ([
    OtpErlangString ("this"); OtpErlangString ("is")]);
    OtpErlangList ([OtpErlangList ([OtpErlangString ("a")])]);
    OtpErlangString ("test")]) in
  assert (
    (binary_ok (term_to_binary list3)) =
    "\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04\x74\x68" ^
    "\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00\x01\x6C\x00\x00" ^
    "\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00\x04\x74\x65\x73\x74\x6A") ;
  true

let test_encode_list () =
  assert (
    (binary_ok (term_to_binary (OtpErlangList ([OtpErlangList ([])])))) =
    "\x83l\x00\x00\x00\x01jj") ;
  let list1 = OtpErlangList ([
    OtpErlangList ([]); OtpErlangList ([]); OtpErlangList ([]);
    OtpErlangList ([]); OtpErlangList ([])]) in
  assert (
    (binary_ok (term_to_binary list1)) =
    "\x83l\x00\x00\x00\x05jjjjjj") ;
  true

let test_encode_improper_list () =
  let list1 = OtpErlangListImproper ([
    OtpErlangTuple ([]); OtpErlangTuple ([])]) in
  assert (
    (binary_ok (term_to_binary list1)) =
    "\x83l\x00\x00\x00\x01h\x00h\x00") ;
  let list2 = OtpErlangListImproper ([
    OtpErlangInteger (0); OtpErlangInteger (1)]) in
  assert (
    (binary_ok (term_to_binary list2)) =
    "\x83l\x00\x00\x00\x01a\x00a\x01") ;
  true

let test_encode_unicode () =
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("")))) =
    "\x83j") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("test")))) =
    "\x83k\x00\x04test") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("\x00\xc3\xbf")))) =
    "\x83k\x00\x03\x00\xc3\xbf") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("\xc4\x80")))) =
    "\x83k\x00\x02\xc4\x80") ;
  let string1 = OtpErlangString ("\xd1\x82\xd0\xb5\xd1\x81\xd1\x82") in
  assert (
    (binary_ok (term_to_binary string1)) =
    "\x83k\x00\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82") ;
  let string2 = OtpErlangString (string_create 65536 "\xd0\x90") in
  assert (
    (binary_ok (term_to_binary string2)) =
    "\x83l\x00\x02\x00\x00" ^ (string_create 65536 "a\xd0a\x90") ^ "j") ;
  true

let test_encode_atom () =
  assert (
    (binary_ok (term_to_binary (OtpErlangAtom ("")))) =
    "\x83s\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangAtom ("test")))) =
    "\x83s\x04test") ;
  true

let test_encode_string_basic () =
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("")))) =
    "\x83\x6A") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("test")))) =
    "\x83\x6B\x00\x04\x74\x65\x73\x74") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("two words")))) =
    "\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("testing multiple words")))) =
    "\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D\x75\x6C\x74" ^
    "\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString (" ")))) =
    "\x83\x6B\x00\x01\x20") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("  ")))) =
    "\x83\x6B\x00\x02\x20\x20") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("1")))) =
    "\x83\x6B\x00\x01\x31") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("37")))) =
    "\x83\x6B\x00\x02\x33\x37") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("one = 1")))) =
    "\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31") ;
  let string1 = OtpErlangString ("!@#$%^&*()_+-=[]{}\\|;':\",./<>?~`") in
  assert (
    (binary_ok (term_to_binary string1)) =
    "\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28\x29\x5F\x2B" ^
    "\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C" ^
    "\x3E\x3F\x7E\x60") ;
  let string2 = OtpErlangString ("\"\x08\x0c\n\r\t\x0bS\x12") in
  assert (
    (binary_ok (term_to_binary string2)) =
    "\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12") ;
  true

let test_encode_string () =
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("")))) =
    "\x83j") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangString ("test")))) =
    "\x83k\x00\x04test") ;
  true

let test_encode_boolean () =
  assert (
    (binary_ok (term_to_binary (OtpErlangAtomBool (true)))) =
    "\x83s\x04true") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangAtomBool (false)))) =
    "\x83s\x05false") ;
  true

let test_encode_small_integer () =
  assert (
    (binary_ok (term_to_binary (OtpErlangInteger (0)))) =
    "\x83a\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangInteger (255)))) =
    "\x83a\xff") ;
  true

let test_encode_integer () =
  assert (
    (binary_ok (term_to_binary (OtpErlangInteger (-1)))) =
    "\x83b\xff\xff\xff\xff") ;
  assert (
    (binary_ok (term_to_binary
      (OtpErlangInteger (Int32.to_int Int32.min_int)))) =
    "\x83b\x80\x00\x00\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangInteger (256)))) =
    "\x83b\x00\x00\x01\x00") ;
  assert (
    (binary_ok (term_to_binary
      (OtpErlangInteger (Int32.to_int Int32.max_int)))) =
    "\x83b\x7f\xff\xff\xff") ;
  true

let test_encode_small_big_integer () =
  let bigint1 = OtpErlangIntegerBig (
    Big_int.add_int_big_int 1 (Big_int.big_int_of_int32 Int32.max_int)) in
  assert (
    (binary_ok (term_to_binary bigint1)) =
    "\x83n\x04\x00\x00\x00\x00\x80") ;
  let bigint2 = OtpErlangIntegerBig (
    Big_int.add_int_big_int (-1) (Big_int.big_int_of_int32 Int32.min_int)) in
  assert (
    (binary_ok (term_to_binary bigint2)) =
    "\x83n\x04\x01\x01\x00\x00\x80") ;
  true

let test_encode_large_big_integer () =
  let bigint1 = OtpErlangIntegerBig (
    Big_int.power_int_positive_int 2 2040) in
  assert (
    (binary_ok (term_to_binary bigint1)) =
    "\x83o\x00\x00\x01\x00\x00" ^ (String.make 255 '\x00') ^ "\x01") ;
  let bigint2 = OtpErlangIntegerBig (
    Big_int.minus_big_int (Big_int.power_int_positive_int 2 2040)) in
  assert (
    (binary_ok (term_to_binary bigint2)) =
    "\x83o\x00\x00\x01\x00\x01" ^ (String.make 255 '\x00') ^ "\x01") ;
  true

let test_encode_float () =
  assert (
    (binary_ok (term_to_binary (OtpErlangFloat (0.0)))) =
    "\x83F\x00\x00\x00\x00\x00\x00\x00\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangFloat (0.5)))) =
    "\x83F?\xe0\x00\x00\x00\x00\x00\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangFloat (-0.5)))) =
    "\x83F\xbf\xe0\x00\x00\x00\x00\x00\x00") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangFloat (3.1415926)))) =
    "\x83F@\t!\xfbM\x12\xd8J") ;
  assert (
    (binary_ok (term_to_binary (OtpErlangFloat (-3.1415926)))) =
    "\x83F\xc0\t!\xfbM\x12\xd8J") ;
  true

let tests =
  register_printers () ;
[
  "binary_to_term/term_to_binary (pid)", test_pid;
  "binary_to_term/term_to_binary (function)", test_function;
  "binary_to_term/term_to_binary (reference)", test_reference;
  "binary_to_term (basic)", test_decode_basic;
  "binary_to_term (atom)", test_decode_atom;
  "binary_to_term (predefined atom)", test_decode_predefined_atom;
  "binary_to_term (empty list)", test_decode_empty_list;
  "binary_to_term (string list)", test_decode_string_list;
  "binary_to_term (list)", test_decode_list;
  "binary_to_term (improper list)", test_decode_improper_list;
  "binary_to_term (small tuple)", test_decode_small_tuple;
  "binary_to_term (large tuple)", test_decode_large_tuple;
  "binary_to_term (small integer)", test_decode_small_integer;
  "binary_to_term (integer, 64bit-only)", test_decode_integer;
  "binary_to_term (binary)", test_decode_binary;
  "binary_to_term (float)", test_decode_float;
  "binary_to_term (small bigint)", test_decode_small_big_integer;
  "binary_to_term (large bigint)", test_decode_large_big_integer;
  "term_to_binary (tuple)", test_encode_tuple;
  "term_to_binary (empty list)", test_encode_empty_list;
  "term_to_binary (string list)", test_encode_string_list;
  "term_to_binary (list basic, 64bit-only)", test_encode_list_basic;
  "term_to_binary (list)", test_encode_list;
  "term_to_binary (improper list)", test_encode_improper_list;
  "term_to_binary (unicode)", test_encode_unicode;
  "term_to_binary (atom)", test_encode_atom;
  "term_to_binary (string basic)", test_encode_string_basic;
  "term_to_binary (string)", test_encode_string;
  "term_to_binary (boolean)", test_encode_boolean;
  "term_to_binary (small integer)", test_encode_small_integer;
  "term_to_binary (integer, 64bit-only)", test_encode_integer;
  "term_to_binary (small bigint)", test_encode_small_big_integer;
  "term_to_binary (large bigint)", test_encode_large_big_integer;
  "term_to_binary (float)", test_encode_float;
]

