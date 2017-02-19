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

let message_init = 1
let message_send_async = 2
let message_send_sync = 3
let message_recv_async = 4
let message_return_async = 5
let message_return_sync = 6
let message_returns_async = 7
let message_keepalive = 8
let message_reinit = 9
let message_subscribe_count = 10
let message_term = 11

type request_type =
    ASYNC
  | SYNC
type source = Erlang.Pid.t

type response =
    Null
  | ResponseInfo of string * string
  | Response of string
  | NullError of string

module Instance = struct
  type t = {
      socket : Unix.file_descr;
      use_header : bool;
      mutable initialization_complete : bool;
      mutable terminate : bool;
      fragment_size : int;
      fragment_recv : bytes;
      callbacks : (string, (
        t -> request_type ->
        string -> string ->
        string -> string ->
        int -> int -> string -> source ->
        response) Queue.t) Hashtbl.t;
      buffer_recv : Buffer.t;
      mutable process_index : int;
      mutable process_count : int;
      mutable process_count_max : int;
      mutable process_count_min : int;
      mutable prefix : string;
      mutable timeout_initialize : int;
      mutable timeout_async : int;
      mutable timeout_sync : int;
      mutable timeout_terminate : int;
      mutable priority_default : int;
      mutable request_timeout_adjustment : bool;
      mutable request_timer : float;
      mutable request_timeout : int;
      mutable response_info : string;
      mutable response : string;
      mutable trans_id : string;
      mutable trans_ids : string array;
      mutable subscribe_count : int;
    }
  let make ~socket ~use_header ~fragment_size ~fragment_recv
    ~callbacks ~buffer_recv ~timeout_terminate =
    {socket; use_header;
     initialization_complete = false;
     terminate = false;
     fragment_size; fragment_recv; callbacks; buffer_recv;
     process_index = 0;
     process_count = 0;
     process_count_max = 0;
     process_count_min = 0;
     prefix = "";
     timeout_initialize = 0;
     timeout_async = 0;
     timeout_sync = 0;
     timeout_terminate;
     priority_default = 0;
     request_timeout_adjustment = false;
     request_timer = 0.0;
     request_timeout = 0;
     response_info = "";
     response = "";
     trans_id = "";
     trans_ids = Array.make 0 "";
     subscribe_count = 0}
  let init api process_index process_count
    process_count_max process_count_min prefix
    timeout_initialize timeout_async timeout_sync timeout_terminate
    priority_default request_timeout_adjustment =
    api.process_index <- process_index ;
    api.process_count <- process_count ;
    api.process_count_max <- process_count_max ;
    api.process_count_min <- process_count_min ;
    api.prefix <- prefix ;
    api.timeout_initialize <- timeout_initialize ;
    api.timeout_async <- timeout_async ;
    api.timeout_sync <- timeout_sync ;
    api.timeout_terminate <- timeout_terminate ;
    api.priority_default <- priority_default ;
    api.request_timeout_adjustment <- request_timeout_adjustment ;
    ()
  let reinit api process_count timeout_async timeout_sync
    priority_default request_timeout_adjustment =
    api.process_count <- process_count ;
    api.timeout_async <- timeout_async ;
    api.timeout_sync <- timeout_sync ;
    api.priority_default <- priority_default ;
    api.request_timeout_adjustment <- request_timeout_adjustment ;
    ()
end

type callback = (
  Instance.t -> request_type ->
  string -> string ->
  string -> string ->
  int -> int -> string -> source ->
  response)

let invalid_input_error = "Invalid Input"
let message_decoding_error = "Message Decoding Error"
let terminate_error = "Terminate"

exception ReturnSync
exception ReturnAsync
exception ForwardSync
exception ForwardAsync

let null_response _ _ _ _ _ _ _ _ _ _ =
  Null

let getenv (key : string) : string =
  try Sys.getenv key with
  Not_found -> ""

let getenv_to_uint (key : string) : (int, string) result =
  try Ok (int_of_string (Sys.getenv key)) with
  _ -> Error (invalid_input_error)

let fd_of_int (fd: int) : Unix.file_descr = Obj.magic fd

let unpack_uint32_native i binary : (int, string) result =
  let byte0 = int_of_char binary.[i + (if Sys.big_endian then 0 else 3)]
  and byte1 = int_of_char binary.[i + (if Sys.big_endian then 1 else 2)]
  and byte2 = int_of_char binary.[i + (if Sys.big_endian then 2 else 1)]
  and byte3 = int_of_char binary.[i + (if Sys.big_endian then 3 else 0)] in
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

let unpack_uint32_big i binary : (int, string) result =
  let byte0 = int_of_char binary.[i]
  and byte1 = int_of_char binary.[i + 1]
  and byte2 = int_of_char binary.[i + 2]
  and byte3 = int_of_char binary.[i + 3] in
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

let pack_uint32_big (value : int) buffer : unit =
  let byte0 = (value lsr 24) land 0xff
  and byte1 = (value lsr 16) land 0xff
  and byte2 = (value lsr 8) land 0xff
  and byte3 = value land 0xff in
  Buffer.add_char buffer (char_of_int byte0) ;
  Buffer.add_char buffer (char_of_int byte1) ;
  Buffer.add_char buffer (char_of_int byte2) ;
  Buffer.add_char buffer (char_of_int byte3)

let send {Instance.socket; use_header; _} data : (unit, string) result =
  let length = String.length data in
  let sent = if not use_header then
    (Unix.write_substring socket data 0 length) = length
  else (
    let total = 4 + length in
    let buffer = Buffer.create total in
    pack_uint32_big length buffer ;
    Buffer.add_string buffer data ;
    (Unix.write socket (Buffer.to_bytes buffer) 0 total) = total)
  in
  if sent then
    Ok (())
  else
    Error ("send failed")

let recv api : (string * int, string) result =
  let {Instance.socket; use_header;
    fragment_size; fragment_recv; buffer_recv; _} = api in
  if use_header then (
    let rec get_header () =
      if (Buffer.length buffer_recv) >= 4 then
        unpack_uint32_big 0 (Buffer.sub buffer_recv 0 4)
      else
        let i = Unix.read socket fragment_recv 0 fragment_size in
        if i = 0 then  
          Error ("recv failed")
        else (
          Buffer.add_subbytes buffer_recv fragment_recv 0 i ;
          get_header ()) in
    let rec get_body (total : int) =
      if (Buffer.length buffer_recv) >= total then (
        let data = (Buffer.sub buffer_recv 4 (total - 4))
        and data_remaining =
          Buffer.sub buffer_recv total ((Buffer.length buffer_recv) - total) in
        Buffer.clear buffer_recv ;
        Buffer.add_string buffer_recv data_remaining ;
        Ok (data))
      else
        let i = Unix.read socket fragment_recv 0 fragment_size in
        if i = 0 then  
          Error ("recv failed")
        else (
          Buffer.add_subbytes buffer_recv fragment_recv 0 i ;
          get_body total) in
    match get_header () with
    | Error (error) ->
      Error (error)
    | Ok (length) ->
      match get_body (4 + length) with
      | Error (error) ->
        Error (error)
      | Ok (data) ->
        Ok ((data, length)))
  else (
    let rec get_body () =
      let i = Unix.read socket fragment_recv 0 fragment_size in
      Buffer.add_subbytes buffer_recv fragment_recv 0 i ;
      let ready = if i = fragment_size then
        let (reading, _, _) = Unix.select [socket] [] [] 0.0 in
        (List.length reading) > 0
      else
        false
      in
      if ready then
        get_body ()
      else (
        let data_all = Buffer.contents buffer_recv in
        Buffer.clear buffer_recv ;
        data_all) in
    let data = get_body () in
    if (String.length data) = 0 then
      Error ("recv failed")
    else
      Ok ((data, String.length data)))

let handle_events api ext data data_size i cmd : (bool, string) result =
  let i_cmd =
    if cmd = 0 then
      match unpack_uint32_native i data with
      | Error (error) ->
        Error (error)
      | Ok (value) ->
        Ok ((i + 4, value))
    else
      Ok ((i, cmd)) in
  match i_cmd with
  | Error (error) ->
    Error (error)
  | Ok ((i0, cmd_value)) ->
    let rec loop i1 cmd_event =
      if cmd_event = message_term then (
        api.Instance.terminate <- true ;
        if ext then
          Ok (false)
        else
          Error (terminate_error))
      else if cmd_event = message_reinit then (
        match unpack_uint32_native i1 data with
        | Error (error) ->
          Error (error)
        | Ok (process_count) ->
          match unpack_uint32_native (i1 + 4) data with
          | Error (error) ->
            Error (error)
          | Ok (timeout_async) ->
            match unpack_uint32_native (i1 + 8) data with
            | Error (error) ->
              Error (error)
            | Ok (timeout_sync) ->
              let priority_default = int_of_char data.[i1 + 12]
              and request_timeout_adjustment = int_of_char data.[i1 + 13]
              and i2 = i1 + 14 in
              Instance.reinit api
                process_count
                timeout_async
                timeout_sync
                priority_default
                (request_timeout_adjustment <> 0) ;
              loop_cmd i2)
      else if cmd_event = message_keepalive then
        match Erlang.term_to_binary (Erlang.OtpErlangAtom ("keepalive")) with
        | Error (error) ->
          Error (error)
        | Ok (keepalive) ->
          match send api keepalive with
          | Error (error) ->
            Error (error)
          | Ok _ ->
            loop_cmd i1
      else
        Error (message_decoding_error)
    and loop_cmd i1 =
      if i1 > data_size then
        Error (message_decoding_error)
      else if i1 = data_size then
        Ok (true)
      else
        match unpack_uint32_native i1 data with
        | Error (error) ->
          Error (error)
        | Ok (cmd_next) ->
          loop (i1 + 4) cmd_next in
    loop i0 cmd_value

let rec poll_request_loop api timeout ext : (bool, string) result = 
  let {Instance.socket; _} = api
  and (poll_timer, timeout_value) =
    if timeout < 0 then
      (0.0, -1.0)
    else
      (Unix.gettimeofday (), (float_of_int timeout) *. 0.001) in
  let (reading, _, excepting) =
    Unix.select [socket] [] [socket] timeout_value in
  if (List.length excepting) > 0 then
    Ok (false)
  else if (List.length reading) = 0 then
    Ok (true)
  else
    match recv api with
    | Error (error) ->
      Error (error)
    | Ok (data, data_size) ->
      match poll_request_data api ext data data_size with
      | Error (error) ->
        Error (error)
      | Ok (Some value) ->
        Ok (value)
      | Ok (None) ->
        let timeout_new = if timeout > 0 then
          let elapsed = truncate
            (((Unix.gettimeofday ()) -. poll_timer) *. 1000.0) in
          if elapsed >= timeout then
            0
          else
            timeout - elapsed
        else
          timeout
        in
        if timeout = 0 then
          Ok (true)
        else
          poll_request_loop api timeout_new ext

and poll_request_data api ext data data_size : (bool option, string) result = 
  match unpack_uint32_native 0 data with
  | Error (error) ->
    Error (error)
  | Ok (cmd) ->
    if cmd = message_init then (
      match unpack_uint32_native 4 data with
      | Error (error) ->
        Error (error)
      | Ok (process_index) ->
        match unpack_uint32_native 8 data with
        | Error (error) ->
          Error (error)
        | Ok (process_count) ->
          match unpack_uint32_native 12 data with
          | Error (error) ->
            Error (error)
          | Ok (process_count_max) ->
            match unpack_uint32_native 16 data with
            | Error (error) ->
              Error (error)
            | Ok (process_count_min) ->
              match unpack_uint32_native 20 data with
              | Error (error) ->
                Error (error)
              | Ok (prefix_size) ->
                let prefix = String.sub data 24 (prefix_size - 1)
                and i0 = 24 + prefix_size in
                match unpack_uint32_native i0 data with
                | Error (error) ->
                  Error (error)
                | Ok (timeout_initialize) ->
                  match unpack_uint32_native (i0 + 4) data with
                  | Error (error) ->
                    Error (error)
                  | Ok (timeout_async) ->
                    match unpack_uint32_native (i0 + 8) data with
                    | Error (error) ->
                      Error (error)
                    | Ok (timeout_sync) ->
                      match unpack_uint32_native (i0 + 12) data with
                      | Error (error) ->
                        Error (error)
                      | Ok (timeout_terminate) ->
                        let priority_default =
                          int_of_char data.[i0 + 16]
                        and request_timeout_adjustment =
                          int_of_char data.[i0 + 17]
                        and i1 = i0 + 18 in
                        Instance.init api
                          process_index
                          process_count
                          process_count_max
                          process_count_min
                          prefix
                          timeout_initialize
                          timeout_async
                          timeout_sync
                          timeout_terminate
                          priority_default
                          (request_timeout_adjustment <> 0) ;
                        if i1 != data_size then
                          match handle_events api ext data data_size i1 0 with
                          | Error (error) ->
                            Error (error)
                          | Ok _ ->
                            Ok (Some false)
                        else
                          Ok (Some false))
    else if (cmd = message_send_async || cmd = message_send_sync) then
      Error ("invalid")
    else if (cmd = message_recv_async || cmd = message_return_sync) then
      Error ("invalid")
    else if cmd = message_return_async then
      Error ("invalid")
    else if cmd = message_returns_async then
      Error ("invalid")
    else if cmd = message_subscribe_count then
      Error ("invalid")
    else if cmd = message_term then
      Error ("invalid")
    else if cmd = message_reinit then
      Error ("invalid")
    else if cmd = message_keepalive then
      Error ("invalid")
    else
      Error (message_decoding_error)

let poll_request api timeout ext : (bool, string) result = 
  let {Instance.initialization_complete; terminate; _} = api in
  if terminate then
    Ok (false)
  else if ext && not initialization_complete then (
    match Erlang.term_to_binary (Erlang.OtpErlangAtom ("polling")) with
    | Error (error) ->
      Error (error)
    | Ok (polling) ->
      match send api polling with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        api.Instance.initialization_complete <- true ;
        poll_request_loop api timeout ext)
  else
    poll_request_loop api timeout ext

let api (thread_index : int) : (Instance.t, string) result =
  let protocol = getenv "CLOUDI_API_INIT_PROTOCOL" in
  if protocol = "" then
    Error (invalid_input_error)
  else
    match getenv_to_uint "CLOUDI_API_INIT_BUFFER_SIZE" with
      | Error (error) ->
        Error (error)
      | Ok (buffer_size) ->
        let socket = fd_of_int (thread_index + 3)
        and use_header = (protocol <> "udp")
        and fragment_size = buffer_size
        and fragment_recv = Bytes.create buffer_size
        and callbacks = Hashtbl.create 1024
        and buffer_recv = Buffer.create buffer_size
        and timeout_terminate = 1000 in
        let api = Instance.make
          ~socket:socket
          ~use_header:use_header
          ~fragment_size:fragment_size
          ~fragment_recv:fragment_recv
          ~callbacks:callbacks
          ~buffer_recv:buffer_recv
          ~timeout_terminate:timeout_terminate in
        match Erlang.term_to_binary (Erlang.OtpErlangAtom ("init")) with
        | Error (error) ->
          Error (error)
        | Ok (init) ->
          match send api init with
          | Error (error) ->
            Error (error)
          | Ok _ ->
            match poll_request api (-1) false with
            | Error (error) ->
              Error (error)
            | Ok _ ->
              Ok (api)

let thread_count () : (int, string) result =
  getenv_to_uint "CLOUDI_API_INIT_THREAD_COUNT"

let process_index (api : Instance.t) : int =
  api.Instance.process_index

let process_count (api : Instance.t) : int =
  api.Instance.process_count

let process_count_max (api : Instance.t) : int =
  api.Instance.process_count_max

let process_count_min (api : Instance.t) : int =
  api.Instance.process_count_min

let prefix (api : Instance.t) : string =
  api.Instance.prefix

let timeout_initialize (api : Instance.t) : int =
  api.Instance.timeout_initialize

let timeout_async (api : Instance.t) : int =
  api.Instance.timeout_async

let timeout_sync (api : Instance.t) : int =
  api.Instance.timeout_sync

let timeout_terminate (api : Instance.t) : int =
  api.Instance.timeout_terminate

let poll (api : Instance.t) (timeout : int) : (bool, string) result =
  poll_request api timeout true

