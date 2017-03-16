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
    Response of string
  | ResponseInfo of string * string
  | Null
  | NullError of string

module Instance = struct
  type 's t = {
      mutable state : 's;
      socket : Unix.file_descr;
      use_header : bool;
      mutable initialization_complete : bool;
      mutable terminate : bool;
      fragment_size : int;
      fragment_recv : bytes;
      callbacks : (string, (
        request_type ->
        string -> string ->
        string -> string ->
        int -> int -> string -> source ->
        's -> 's t ->
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
  let make ~state ~socket ~use_header ~fragment_size ~fragment_recv
    ~callbacks ~buffer_recv ~timeout_terminate =
    {state; socket; use_header;
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
  let set_response api response_info response trans_id =
    api.response_info <- response_info ;
    api.response <- response ;
    api.trans_id <- trans_id ;
    ()
  let set_trans_id api trans_id =
    api.trans_id <- trans_id ;
    ()
  let set_trans_ids api trans_ids_str trans_id_count =
    api.trans_ids <- Array.init trans_id_count (fun i ->
      String.sub trans_ids_str (i * 16) 16
    ) ;
    ()
  let set_subscribe_count api count =
    api.subscribe_count <- count ;
    ()
  let callbacks_add api pattern f =
    let key = api.prefix ^ pattern in
    let value = try Hashtbl.find api.callbacks key
    with Not_found -> (
      let value_new = Queue.create () in
      Hashtbl.add api.callbacks key value_new ;
      value_new)
    in
    Queue.push f value ;
    ()
  let callbacks_remove api pattern =
    let key = api.prefix ^ pattern in
    let value = Hashtbl.find api.callbacks key in
    let _ = Queue.pop value in
    if Queue.is_empty value then
      Hashtbl.remove api.callbacks key ;
    ()
end

type 's callback = (
  request_type ->
  string -> string ->
  string -> string ->
  int -> int -> string -> source ->
  's -> 's Instance.t ->
  response)

let trans_id_null = String.make 16 '\x00'

let invalid_input_error = "Invalid Input"
let message_decoding_error = "Message Decoding Error"
let terminate_error = "Terminate"

exception ReturnSync
exception ReturnAsync
exception ForwardSync
exception ForwardAsync

let print_exception s =
  prerr_endline ("Exception: " ^ s)

let print_error s =
  prerr_endline ("Error: " ^ s)

let str_replace input output =
  Str.global_replace (Str.regexp_string input) output

let str_split_on_char sep s =
  (* based on https://github.com/ocaml/ocaml/blob/trunk/stdlib/string.ml
   * (split_on_char) for use with OCaml 4.03.0
   *)
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

let list_append l1 l2 = List.rev_append (List.rev l1) l2

let backtrace (e : exn) : string =
  let indent = "  " in
  (Printexc.to_string e) ^ "\n" ^ indent ^
  (String.trim (str_replace "\n" ("\n" ^ indent) (Printexc.get_backtrace ())))

let null_response _ _ _ _ _ _ _ _ _ _ _ =
  Null

let getenv (key : string) : string =
  try Sys.getenv key
  with Not_found -> ""

let getenv_to_uint (key : string) : (int, string) result =
  try Ok (int_of_string (Sys.getenv key))
  with _ -> Error (invalid_input_error)

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

let unpack_int8 i binary : int =
  let byte0 = int_of_char binary.[i] in
  if (byte0 lsr 7) = 1 then
    -128 + (0x7f land byte0)
  else
    byte0

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
          get_header ())
    in
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
          get_body total)
    in
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
        data_all)
    in
    let data = get_body () in
    if (String.length data) = 0 then
      Error ("recv failed")
    else
      Ok ((data, String.length data)))

let forward_async
  api name request_info request timeout priority trans_id pid :
  (unit, string) result =
  let {Instance.request_timeout_adjustment;
    request_timer; request_timeout; _} = api in
  let timeout_new =
    if request_timeout_adjustment && timeout = request_timeout then
      let elapsed = truncate
        (((Unix.gettimeofday ()) -. request_timer) *. 1000.0) in
      if elapsed < 0 then
        timeout
      else if elapsed >= timeout then
        0
      else
        timeout - elapsed
    else
      timeout
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("forward_async");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangBinary (request_info);
      Erlang.OtpErlangBinary (request);
      Erlang.OtpErlangInteger (timeout_new);
      Erlang.OtpErlangInteger (priority);
      Erlang.OtpErlangBinary (trans_id);
      Erlang.OtpErlangPid (pid)])) with
  | Error (error) ->
    Error (error)
  | Ok (forward) ->
    match send api forward with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      raise ForwardAsync

let forward_sync
  api name request_info request timeout priority trans_id pid :
  (unit, string) result =
  let {Instance.request_timeout_adjustment;
    request_timer; request_timeout; _} = api in
  let timeout_new =
    if request_timeout_adjustment && timeout = request_timeout then
      let elapsed = truncate
        (((Unix.gettimeofday ()) -. request_timer) *. 1000.0) in
      if elapsed < 0 then
        timeout
      else if elapsed >= timeout then
        0
      else
        timeout - elapsed
    else
      timeout
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("forward_sync");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangBinary (request_info);
      Erlang.OtpErlangBinary (request);
      Erlang.OtpErlangInteger (timeout_new);
      Erlang.OtpErlangInteger (priority);
      Erlang.OtpErlangBinary (trans_id);
      Erlang.OtpErlangPid (pid)])) with
  | Error (error) ->
    Error (error)
  | Ok (forward) ->
    match send api forward with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      raise ForwardSync

let forward_
  api type_ name request_info request timeout priority trans_id pid :
  (unit, string) result =
  match type_ with
  | ASYNC ->
    forward_async api name request_info request timeout priority trans_id pid
  | SYNC ->
    forward_sync api name request_info request timeout priority trans_id pid

let return_async_i
  api name pattern response_info response timeout trans_id pid :
  (unit, string) result =
  let {Instance.request_timeout_adjustment;
    request_timer; request_timeout; _} = api in
  let timeout_new =
    if request_timeout_adjustment && timeout = request_timeout then
      let elapsed = truncate
        (((Unix.gettimeofday ()) -. request_timer) *. 1000.0) in
      if elapsed < 0 then
        timeout
      else if elapsed >= timeout then
        0
      else
        timeout - elapsed
    else
      timeout
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("return_async");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangString (pattern);
      Erlang.OtpErlangBinary (response_info);
      Erlang.OtpErlangBinary (response);
      Erlang.OtpErlangInteger (timeout_new);
      Erlang.OtpErlangBinary (trans_id);
      Erlang.OtpErlangPid (pid)])) with
  | Error (error) ->
    Error (error)
  | Ok (return) ->
    send api return

let return_async
  api name pattern response_info response timeout trans_id pid :
  (unit, string) result =
  match return_async_i
    api name pattern response_info response timeout trans_id pid with
  | Error (error) ->
    Error (error)
  | Ok _ ->
    raise ReturnAsync

let return_sync_i
  api name pattern response_info response timeout trans_id pid :
  (unit, string) result =
  let {Instance.request_timeout_adjustment;
    request_timer; request_timeout; _} = api in
  let timeout_new =
    if request_timeout_adjustment && timeout = request_timeout then
      let elapsed = truncate
        (((Unix.gettimeofday ()) -. request_timer) *. 1000.0) in
      if elapsed < 0 then
        timeout
      else if elapsed >= timeout then
        0
      else
        timeout - elapsed
    else
      timeout
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("return_sync");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangString (pattern);
      Erlang.OtpErlangBinary (response_info);
      Erlang.OtpErlangBinary (response);
      Erlang.OtpErlangInteger (timeout_new);
      Erlang.OtpErlangBinary (trans_id);
      Erlang.OtpErlangPid (pid)])) with
  | Error (error) ->
    Error (error)
  | Ok (return) ->
    send api return

let return_sync
  api name pattern response_info response timeout trans_id pid :
  (unit, string) result =
  match return_sync_i
    api name pattern response_info response timeout trans_id pid with
  | Error (error) ->
    Error (error)
  | Ok _ ->
    raise ReturnSync

let return_
  api type_ name pattern response_info response timeout trans_id pid :
  (unit, string) result =
  match type_ with
  | ASYNC ->
    return_async api name pattern response_info response timeout trans_id pid
  | SYNC ->
    return_sync api name pattern response_info response timeout trans_id pid

let handle_events api ext data data_size i cmd : (bool, string) result =
  let i_cmd =
    if cmd = 0 then
      match unpack_uint32_native i data with
      | Error (error) ->
        Error (error)
      | Ok (value) ->
        Ok ((i + 4, value))
    else
      Ok ((i, cmd))
  in
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
              let priority_default = unpack_int8 (i1 + 12) data
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
          loop (i1 + 4) cmd_next
    in
    loop i0 cmd_value

let rec poll_request_loop api timeout ext poll_timer: (bool, string) result = 
  let {Instance.socket; _} = api
  and timeout_value =
    if timeout < 0 then
      -1.0
    else if timeout = 0 then
      0.0
    else
      (float_of_int timeout) *. 0.001
  in
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
      match poll_request_data api ext data data_size 0 with
      | Error (error) ->
        Error (error)
      | Ok (Some value) ->
        Ok (value)
      | Ok (None) ->
        let poll_timer_new =
          if timeout > 0 then
            Unix.gettimeofday ()
          else
            0.0
        in
        let timeout_new =
          if timeout > 0 then
            let elapsed = truncate
              ((poll_timer_new -. poll_timer) *. 1000.0) in
            if elapsed <= 0 then
              timeout
            else if elapsed >= timeout then
              0
            else
              timeout - elapsed
          else
            timeout
        in
        if timeout = 0 then
          Ok (true)
        else
          poll_request_loop api timeout_new ext poll_timer_new

and callback
  api request_type name pattern request_info request
  timeout priority trans_id pid : (bool option, string) result =
  let {Instance.state; callbacks; request_timeout_adjustment; _} = api in
  if request_timeout_adjustment then (
    api.Instance.request_timer <- Unix.gettimeofday () ;
    api.Instance.request_timeout <- timeout) ;
  let callback_get () =
    let function_queue = Hashtbl.find callbacks pattern in
    let f = Queue.pop function_queue in
    Queue.push f function_queue ;
    f
  in
  let callback_f =
    try callback_get ()
    with Not_found -> null_response
  in
  let callback_result =
    match request_type with
    | ASYNC -> (
      try Some (
        callback_f
          request_type name pattern request_info request
          timeout priority trans_id pid state api)
      with
        | ReturnSync ->
          print_exception "Synchronous Call Return Invalid" ;
          None
        | ReturnAsync ->
          None
        | ForwardSync ->
          print_exception "Synchronous Call Forward Invalid" ;
          None
        | ForwardAsync ->
          None
        | e ->
          print_exception (backtrace e) ;
          Some (Null))
    | SYNC -> (
      try Some (
        callback_f
          request_type name pattern request_info request
          timeout priority trans_id pid state api)
      with
        | ReturnSync ->
          None
        | ReturnAsync ->
          print_exception "Asynchronous Call Return Invalid" ;
          None
        | ForwardSync ->
          None
        | ForwardAsync ->
          print_exception "Asynchronous Call Forward Invalid" ;
          None
        | e ->
          print_exception (backtrace e) ;
          Some (Null))
  in
  let response_result =
    match callback_result with
    | Some (ResponseInfo _) ->
      callback_result
    | Some (Response (value1)) ->
      Some (ResponseInfo ("", value1))
    | Some (Null) ->
      Some (ResponseInfo ("", ""))
    | Some (NullError (error)) ->
      print_error error ;
      Some (ResponseInfo ("", ""))
    | None ->
      None
  in
  let return_result =
    match request_type with
    | ASYNC -> (
      match response_result with
      | None ->
        Ok (())
      | Some (ResponseInfo (response_info, response)) ->
        return_async_i
          api name pattern response_info response timeout trans_id pid
      | Some (Response _ | Null | NullError _) ->
        Error (message_decoding_error))
    | SYNC -> (
      match response_result with
      | None ->
        Ok (())
      | Some (ResponseInfo (response_info, response)) ->
        return_sync_i
          api name pattern response_info response timeout trans_id pid
      | Some (Response _ | Null | NullError _) ->
        Error (message_decoding_error))
  in
  match return_result with
  | Error (error) ->
    Error (error)
  | Ok _ ->
    Ok (None)

and poll_request_data api ext data data_size i : (bool option, string) result = 
  match unpack_uint32_native i data with
  | Error (error) ->
    Error (error)
  | Ok (cmd) ->
    if cmd = message_init then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (process_index) ->
        match unpack_uint32_native (i + 8) data with
        | Error (error) ->
          Error (error)
        | Ok (process_count) ->
          match unpack_uint32_native (i + 12) data with
          | Error (error) ->
            Error (error)
          | Ok (process_count_max) ->
            match unpack_uint32_native (i + 16) data with
            | Error (error) ->
              Error (error)
            | Ok (process_count_min) ->
              match unpack_uint32_native (i + 20) data with
              | Error (error) ->
                Error (error)
              | Ok (prefix_size) ->
                let i0 = i + 24 in
                let prefix = String.sub data i0 (prefix_size - 1)
                and i1 = i0 + prefix_size in
                match unpack_uint32_native i1 data with
                | Error (error) ->
                  Error (error)
                | Ok (timeout_initialize) ->
                  match unpack_uint32_native (i1 + 4) data with
                  | Error (error) ->
                    Error (error)
                  | Ok (timeout_async) ->
                    match unpack_uint32_native (i1 + 8) data with
                    | Error (error) ->
                      Error (error)
                    | Ok (timeout_sync) ->
                      match unpack_uint32_native (i1 + 12) data with
                      | Error (error) ->
                        Error (error)
                      | Ok (timeout_terminate) ->
                        let priority_default =
                          unpack_int8 (i1 + 16) data
                        and request_timeout_adjustment =
                          int_of_char data.[i1 + 17]
                        and i2 = i1 + 18 in
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
                        if i2 <> data_size then
                          match handle_events api ext data data_size i2 0 with
                          | Error (error) ->
                            Error (error)
                          | Ok _ ->
                            Ok (Some false)
                        else
                          Ok (Some false)
    else if cmd = message_send_async || cmd = message_send_sync then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (name_size) ->
        let i0 = i + 8 in
        let name = String.sub data i0 (name_size - 1)
        and i1 = i0 + name_size in
        match unpack_uint32_native i1 data with
        | Error (error) ->
          Error (error)
        | Ok (pattern_size) ->
          let i2 = i1 + 4 in
          let pattern = String.sub data i2 (pattern_size - 1)
          and i3 = i2 + pattern_size in
          match unpack_uint32_native i3 data with
          | Error (error) ->
            Error (error)
          | Ok (request_info_size) ->
            let i4 = i3 + 4 in
            let request_info = String.sub data i4 request_info_size
            and i5 = i4 + request_info_size + 1 in
            match unpack_uint32_native i5 data with
            | Error (error) ->
              Error (error)
            | Ok (request_size) ->
              let i6 = i5 + 4 in
              let request = String.sub data i6 request_size
              and i7 = i6 + request_size + 1 in
              match unpack_uint32_native i7 data with
              | Error (error) ->
                Error (error)
              | Ok (timeout) ->
                let priority = unpack_int8 (i7 + 4) data
                and trans_id = String.sub data (i7 + 5) 16
                and i8 = i7 + 4 + 1 + 16 in
                match unpack_uint32_native i8 data with
                | Error (error) ->
                  Error (error)
                | Ok (pid_size) ->
                  let i9 = i8 + 4 in
                  let pid_data = String.sub data i9 pid_size
                  and i10 = i9 + pid_size in
                  match Erlang.binary_to_term pid_data with
                  | Error (error) ->
                    Error (error)
                  | Ok (
                      Erlang.OtpErlangInteger _
                    | Erlang.OtpErlangIntegerBig _
                    | Erlang.OtpErlangFloat _
                    | Erlang.OtpErlangAtom _
                    | Erlang.OtpErlangAtomUTF8 _
                    | Erlang.OtpErlangAtomCacheRef _
                    | Erlang.OtpErlangAtomBool _
                    | Erlang.OtpErlangString _
                    | Erlang.OtpErlangBinary _
                    | Erlang.OtpErlangBinaryBits (_, _)
                    | Erlang.OtpErlangList _
                    | Erlang.OtpErlangListImproper _
                    | Erlang.OtpErlangTuple _
                    | Erlang.OtpErlangMap _
                    | Erlang.OtpErlangPort _
                    | Erlang.OtpErlangReference _
                    | Erlang.OtpErlangFunction _) ->
                    Error (message_decoding_error)
                  | Ok (Erlang.OtpErlangPid (pid)) ->
                    let handled =
                      if i10 <> data_size then
                        handle_events api ext data data_size i10 0
                      else
                        Ok (true)
                    in
                    match handled with
                    | Error (error) ->
                      Error (error)
                    | Ok (false)  ->
                      Ok (Some false)
                    | Ok (true)  ->
                      let request_type =
                        if cmd = message_send_async then
                          ASYNC
                        else (* cmd = message_send_sync *)
                          SYNC
                      in
                      callback
                        api request_type name pattern request_info request
                        timeout priority trans_id pid
    else if cmd = message_recv_async || cmd = message_return_sync then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (response_info_size) ->
        let i0 = i + 8 in
        let response_info = String.sub data i0 response_info_size
        and i1 = i0 + response_info_size + 1 in
        match unpack_uint32_native i1 data with
        | Error (error) ->
          Error (error)
        | Ok (response_size) ->
          let i2 = i1 + 4 in
          let response = String.sub data i2 response_size
          and i3 = i2 + response_size + 1 in
          let trans_id = String.sub data i3 16
          and i4 = i3 + 16 in
          Instance.set_response api
            response_info
            response
            trans_id ;
          if i4 <> data_size then
            match handle_events api ext data data_size i4 0 with
            | Error (error) ->
              Error (error)
            | Ok _ ->
              Ok (Some false)
          else
            Ok (Some false)
    else if cmd = message_return_async then (
      let i0 = i + 4 in
      let trans_id = String.sub data i0 16
      and i1 = i0 + 16 in
      Instance.set_trans_id api
        trans_id ;
      if i1 <> data_size then
        match handle_events api ext data data_size i1 0 with
        | Error (error) ->
          Error (error)
        | Ok _ ->
          Ok (Some false)
      else
        Ok (Some false))
    else if cmd = message_returns_async then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (trans_id_count) ->
        let i0 = i + 8
        and trans_ids_str_size = 16 * trans_id_count in
        let trans_ids_str = String.sub data i0 trans_ids_str_size
        and i1 = i0 + trans_ids_str_size in
        Instance.set_trans_ids api
          trans_ids_str
          trans_id_count ;
        if i1 <> data_size then
          match handle_events api ext data data_size i1 0 with
          | Error (error) ->
            Error (error)
          | Ok _ ->
            Ok (Some false)
        else
          Ok (Some false)
    else if cmd = message_subscribe_count then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (count) ->
        let i0 = i + 8 in
        Instance.set_subscribe_count api
          count ;
        if i0 <> data_size then
          match handle_events api ext data data_size i0 0 with
          | Error (error) ->
            Error (error)
          | Ok _ ->
            Ok (Some false)
        else
          Ok (Some false)
    else if cmd = message_term then
      match handle_events api ext data data_size (i + 4) cmd with
      | Error (error) ->
        Error (error)
      | Ok (true) ->
        Error (message_decoding_error)
      | Ok (false) ->
        Ok (Some false)
    else if cmd = message_reinit then
      match unpack_uint32_native (i + 4) data with
      | Error (error) ->
        Error (error)
      | Ok (process_count) ->
        match unpack_uint32_native (i + 8) data with
        | Error (error) ->
          Error (error)
        | Ok (timeout_async) ->
          match unpack_uint32_native (i + 12) data with
          | Error (error) ->
            Error (error)
          | Ok (timeout_sync) ->
            let priority_default = unpack_int8 (i + 16) data
            and request_timeout_adjustment = int_of_char data.[i + 17]
            and i1 = i + 18 in
            Instance.reinit api
              process_count
              timeout_async
              timeout_sync
              priority_default
              (request_timeout_adjustment <> 0) ;
            if i1 = data_size then
              Ok (None)
            else if i1 < data_size then
              poll_request_data api ext data data_size i1
            else
              Error (message_decoding_error)
    else if cmd = message_keepalive then
      match Erlang.term_to_binary (Erlang.OtpErlangAtom ("keepalive")) with
      | Error (error) ->
        Error (error)
      | Ok (keepalive) ->
        match send api keepalive with
        | Error (error) ->
          Error (error)
        | Ok _ ->
          let i1 = i + 4 in
          if i1 = data_size then
            Ok (None)
          else if i1 < data_size then
            poll_request_data api ext data data_size i1
          else
            Error (message_decoding_error)
    else
      Error (message_decoding_error)

let poll_request api timeout ext : (bool, string) result = 
  let {Instance.initialization_complete; terminate; _} = api in
  if terminate then
    Ok (false)
  else
    let poll_timer =
      if timeout > 0 then
        Unix.gettimeofday ()
      else
        0.0
    in
    if ext && not initialization_complete then (
      match Erlang.term_to_binary (Erlang.OtpErlangAtom ("polling")) with
      | Error (error) ->
        Error (error)
      | Ok (polling) ->
        match send api polling with
        | Error (error) ->
          Error (error)
        | Ok _ ->
          api.Instance.initialization_complete <- true ;
          poll_request_loop api timeout ext poll_timer)
    else
      poll_request_loop api timeout ext poll_timer

let api (thread_index : int) (state : 's): ('s Instance.t, string) result =
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
          ~state:state
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

let subscribe api pattern f =
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("subscribe");
      Erlang.OtpErlangString (pattern)])) with
  | Error (error) ->
    Error (error)
  | Ok (subscribe) ->
    match send api subscribe with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      Instance.callbacks_add api pattern f ;
      Ok (())

let subscribe_count api pattern =
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("subscribe_count");
      Erlang.OtpErlangString (pattern)])) with
  | Error (error) ->
    Error (error)
  | Ok (subscribe_count) ->
    match send api subscribe_count with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      match poll_request api (-1) false with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        Ok (api.Instance.subscribe_count)

let unsubscribe api pattern =
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("unsubscribe");
      Erlang.OtpErlangString (pattern)])) with
  | Error (error) ->
    Error (error)
  | Ok (unsubscribe) ->
    match send api unsubscribe with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      Instance.callbacks_remove api pattern ;
      Ok (())

let send_async
  ?timeout:(timeout_arg = -1)
  ?request_info:(request_info = "")
  ?priority:(priority_arg = 256)
  api name request =
  let timeout =
    if timeout_arg = -1 then
      api.Instance.timeout_async
    else
      timeout_arg
  and priority =
    if priority_arg = 256 then
      api.Instance.priority_default
    else
      priority_arg
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("send_async");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangBinary (request_info);
      Erlang.OtpErlangBinary (request);
      Erlang.OtpErlangInteger (timeout);
      Erlang.OtpErlangInteger (priority)])) with
  | Error (error) ->
    Error (error)
  | Ok (send_async) ->
    match send api send_async with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      match poll_request api (-1) false with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        Ok (api.Instance.trans_id)

let send_sync
  ?timeout:(timeout_arg = -1)
  ?request_info:(request_info = "")
  ?priority:(priority_arg = 256)
  api name request =
  let timeout =
    if timeout_arg = -1 then
      api.Instance.timeout_sync
    else
      timeout_arg
  and priority =
    if priority_arg = 256 then
      api.Instance.priority_default
    else
      priority_arg
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("send_sync");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangBinary (request_info);
      Erlang.OtpErlangBinary (request);
      Erlang.OtpErlangInteger (timeout);
      Erlang.OtpErlangInteger (priority)])) with
  | Error (error) ->
    Error (error)
  | Ok (send_sync) ->
    match send api send_sync with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      match poll_request api (-1) false with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        Ok ((
          api.Instance.response_info,
          api.Instance.response,
          api.Instance.trans_id))

let mcast_async
  ?timeout:(timeout_arg = -1)
  ?request_info:(request_info = "")
  ?priority:(priority_arg = 256)
  api name request =
  let timeout =
    if timeout_arg = -1 then
      api.Instance.timeout_async
    else
      timeout_arg
  and priority =
    if priority_arg = 256 then
      api.Instance.priority_default
    else
      priority_arg
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("mcast_async");
      Erlang.OtpErlangString (name);
      Erlang.OtpErlangBinary (request_info);
      Erlang.OtpErlangBinary (request);
      Erlang.OtpErlangInteger (timeout);
      Erlang.OtpErlangInteger (priority)])) with
  | Error (error) ->
    Error (error)
  | Ok (mcast_async) ->
    match send api mcast_async with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      match poll_request api (-1) false with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        Ok (api.Instance.trans_ids)

let recv_async
  ?timeout:(timeout_arg = -1)
  ?trans_id:(trans_id = trans_id_null)
  ?consume:(consume = true)
  api =
  let timeout =
    if timeout_arg = -1 then
      api.Instance.timeout_sync
    else
      timeout_arg
  in
  match Erlang.term_to_binary (
    Erlang.OtpErlangTuple ([
      Erlang.OtpErlangAtom ("recv_async");
      Erlang.OtpErlangInteger (timeout);
      Erlang.OtpErlangBinary (trans_id);
      Erlang.OtpErlangAtomBool (consume)])) with
  | Error (error) ->
    Error (error)
  | Ok (recv_async) ->
    match send api recv_async with
    | Error (error) ->
      Error (error)
    | Ok _ ->
      match poll_request api (-1) false with
      | Error (error) ->
        Error (error)
      | Ok _ ->
        Ok ((
          api.Instance.response_info,
          api.Instance.response,
          api.Instance.trans_id))

let process_index (api : 's Instance.t) : int =
  api.Instance.process_index

let process_count (api : 's Instance.t) : int =
  api.Instance.process_count

let process_count_max (api : 's Instance.t) : int =
  api.Instance.process_count_max

let process_count_min (api : 's Instance.t) : int =
  api.Instance.process_count_min

let prefix (api : 's Instance.t) : string =
  api.Instance.prefix

let timeout_initialize (api : 's Instance.t) : int =
  api.Instance.timeout_initialize

let timeout_async (api : 's Instance.t) : int =
  api.Instance.timeout_async

let timeout_sync (api : 's Instance.t) : int =
  api.Instance.timeout_sync

let timeout_terminate (api : 's Instance.t) : int =
  api.Instance.timeout_terminate

let poll (api : 's Instance.t) (timeout : int) : (bool, string) result =
  poll_request api timeout true

let text_key_value_parse text : (string, string list) Hashtbl.t =
  let result = Hashtbl.create 32
  and data = str_split_on_char '\x00' text in
  let rec loop = function
  | [] ->
    result
  | [""] ->
    result
  | key::t0 ->
    match t0 with
    | [] ->
      raise Exit
    | value::t1 -> (
      try
        let l = Hashtbl.find result key in
        Hashtbl.replace result key (list_append l [value])
      with Not_found ->
        Hashtbl.add result key [value]) ;
      loop t1
  in
  loop data

let request_http_qs_parse request =
  text_key_value_parse request

let info_key_value_parse message_info =
  text_key_value_parse message_info

