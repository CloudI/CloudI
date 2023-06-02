//-*-Mode:rust;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
//ex: set ft=rust fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

//! # Rust CloudI API

// MIT License
//
// Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#![crate_name = "cloudi"]
#![crate_type = "lib"]

extern crate erlang;

use std::backtrace::Backtrace;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::net::UdpSocket;
use std::os::fd::FromRawFd;
use std::os::raw::c_int;
use std::time::Duration;
use std::time::Instant;
use erlang::OtpErlangTerm;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum RequestType {
    ASYNC = 1,
    SYNC = -1,
}
pub type Timeout = u32;
pub type Priority = i8;
pub type Source = OtpErlangTerm;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Response {
    Response(Vec<u8>),
    ResponseInfo(Vec<u8>, Vec<u8>),
    Forward(String, Vec<u8>, Vec<u8>),
    Forward_(String, Vec<u8>, Vec<u8>, Timeout, Priority),
    Null(),
    NullError(&'static str),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct TransId {
    id: [u8; 16],
}

impl TransId {
    pub fn new(id: &[u8]) -> Self {
        TransId {
            id: id.try_into().unwrap(),
        }
    }

    pub fn null() -> Self {
        TransId {
            id: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        }
    }

    pub fn is_timeout(&self) -> bool {
        self == &TransId::null()
    }

    pub fn as_slice(&self) -> &[u8] {
        self.id.as_slice()
    }

    pub fn to_vec(&self) -> Vec<u8> {
        self.id.to_vec()
    }
}

/// a function pointer to handle a service request
pub type Callback<'s, S> = fn(&RequestType,
                              &str,
                              &str,
                              &[u8],
                              &[u8],
                              Timeout,
                              Priority,
                              &TransId,
                              &Source,
                              &mut S,
                              &mut API<'s, S>) -> Response;

/// fatal error macro for using panic!
#[macro_export]
macro_rules! fatal {
    ($($arg:tt)*) => {{
        panic!($($arg)*);
    }};
}

/// Error description
#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
    Terminate { timeout: Timeout },
    InvalidInputError(),
    MessageDecodingError(),
    UnexpectedError(),
}

/// Error data
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    _backtrace: Backtrace,
}

impl Error {
    fn new<E>(error: E) -> Self
    where E: Into<Box<dyn std::error::Error + Send + Sync>>, {
        Error {
            kind: ErrorKind::UnexpectedError(),
            source: Some(error.into()),
            _backtrace: Backtrace::capture(),
        }
    }
}

impl PartialEq<Error> for Error {
    fn eq(&self, other: &Error) -> bool {
        self.kind == (*other).kind
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.source {
            None => None,
            Some(e) => Some(&**e),
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error {
            kind,
            source: None,
            _backtrace: Backtrace::capture(),
        }
    }
}

impl From<erlang::Error> for Error {
    fn from(error: erlang::Error) -> Self {
        Error::new(error)
    }
}

impl From<std::env::VarError> for Error {
    fn from(error: std::env::VarError) -> Self {
        Error::new(error)
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::new(error)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Self {
        Error::new(error)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Self {
        Error::new(error)
    }
}

/// an instance of the CloudI API
pub struct API<'s, S> {
    state: &'s mut S,
    callbacks: HashMap<String, LinkedList<Callback<'s, S>>>,
    buffer_recv: Vec<u8>,
    buffer_size: usize,
    socket_kind: SocketKind,
    process_index: u32,
    process_count: u32,
    process_count_min: u32,
    process_count_max: u32,
    prefix: String,
    timeout_initialize: u32,
    timeout_async: u32,
    timeout_sync: u32,
    timeout_terminate: u32,
    priority_default: i8,
    use_header: bool,
    initialization_complete: bool,
    fatal_exceptions: bool,
    terminate: bool,
    response: Option<(Vec<u8>, Vec<u8>, TransId)>,
    trans_id: Option<TransId>,
    trans_ids: Option<Vec<TransId>>,
    subscribe_count: Option<u32>,
}

/// the number of threads to create per operating system process
pub fn thread_count() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_THREAD_COUNT")
}

impl<'s, S> API<'s, S> {
    /// creates an instance of the CloudI API
    pub fn new(thread_index: u32,
               state: &'s mut S) -> Result<Self> {
        let protocol = match std::env::var("CLOUDI_API_INIT_PROTOCOL") {
            Ok(protocol_str) => protocol_str,
            Err(_) => {
                eprintln!("CloudI service execution must occur in CloudI");
                return Err(ErrorKind::InvalidInputError().into());
            },
        };
        let buffer_size =
            getenv_to_u32("CLOUDI_API_INIT_BUFFER_SIZE")? as usize;
        let buffer_recv = Vec::with_capacity(buffer_size);
        let fd: c_int = (thread_index + 3) as i32;
        let (socket_kind, use_header) = match protocol.as_str() {
            "tcp" | "local" => {
                (SocketKind::TCP(unsafe { TcpStream::from_raw_fd(fd) }), true)
            },
            "udp" => {
                (SocketKind::UDP(unsafe { UdpSocket::from_raw_fd(fd) }), false)
            },
            _ => {
                return Err(ErrorKind::InvalidInputError().into());
            },
        };
        let mut api = API {
            state,
            callbacks: HashMap::new(),
            buffer_recv,
            buffer_size,
            socket_kind,
            process_index: 0,
            process_count: 0,
            process_count_min: 0,
            process_count_max: 0,
            prefix: String::new(),
            timeout_initialize: 0,
            timeout_async: 0,
            timeout_sync: 0,
            timeout_terminate: 10, // TIMEOUT_TERMINATE_MIN
            priority_default: 0,
            use_header,
            initialization_complete: false,
            fatal_exceptions: false,
            terminate: false,
            response: None,
            trans_id: None,
            trans_ids: None,
            subscribe_count: None,
        };
        api.send(&OtpErlangTerm::OtpErlangAtomUTF8(b"init".to_vec()))?;
        let _ = api.poll_request(&None, false)?;
        Ok(api)
    }

    /// subscribes to a service name pattern with a function pointer callback
    /// (or a closure callback without a context)
    pub fn subscribe(&mut self, pattern: &str,
                     f: Callback<'s, S>) -> Result<()> {
        let key = self.prefix.clone() + pattern;
        match self.callbacks.get_mut(&key) {
            None => {
                let mut callback_list = LinkedList::new();
                callback_list.push_back(f);
                let _ = self.callbacks.insert(key, callback_list);
            },
            Some(callback_list) => {
                callback_list.push_back(f);
            },
        };
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"subscribe".to_vec()),
            OtpErlangTerm::OtpErlangString(pattern.into()),
        ]))
    }

    /// determine how many service name pattern subscriptions have occurred
    pub fn subscribe_count(&mut self, pattern: &str) -> Result<u32> {
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"subscribe_count".to_vec()),
            OtpErlangTerm::OtpErlangString(pattern.into()),
        ]))?;
        let _ = self.poll_request(&None, false)?;
        Ok(self.subscribe_count.take().unwrap())
    }

    /// unsubscribes from a service name pattern
    pub fn unsubscribe(&mut self, pattern: &str) -> Result<()> {
        let key = self.prefix.clone() + pattern;
        let callback_list = self.callbacks.get_mut(&key).unwrap();
        let _ = callback_list.pop_front().unwrap();
        if callback_list.is_empty() {
            self.callbacks.remove(&key).unwrap();
        }
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"unsubscribe".to_vec()),
            OtpErlangTerm::OtpErlangString(pattern.into()),
        ]))
    }

    /// asynchronous point-to-point communication to a service
    pub fn send_async(&mut self, name: &str, request: &Vec<u8>,
                      timeout_opt: Option<u32>,
                      request_info_opt: Option<Vec<u8>>,
                      priority_opt: Option<i8>) -> Result<TransId> {
        let timeout = timeout_opt.unwrap_or(self.timeout_async);
        let request_info = request_info_opt.unwrap_or(b"".to_vec());
        let priority = priority_opt.unwrap_or(self.priority_default);
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"send_async".to_vec()),
            OtpErlangTerm::OtpErlangString(name.into()),
            OtpErlangTerm::OtpErlangBinary(request_info),
            OtpErlangTerm::OtpErlangBinary(request.clone()),
            OtpErlangTerm::OtpErlangInteger(timeout as i32),
            OtpErlangTerm::OtpErlangInteger(priority as i32),
        ]))?;
        let _ = self.poll_request(&None, false)?;
        Ok(self.trans_id.take().unwrap())
    }

    /// synchronous point-to-point communication to a service
    pub fn send_sync(&mut self, name: &str, request: &Vec<u8>,
                     timeout_opt: Option<u32>,
                     request_info_opt: Option<Vec<u8>>,
                     priority_opt: Option<i8>) ->
                     Result<(Vec<u8>, Vec<u8>, TransId)> {
        let timeout = timeout_opt.unwrap_or(self.timeout_sync);
        let request_info = request_info_opt.unwrap_or(b"".to_vec());
        let priority = priority_opt.unwrap_or(self.priority_default);
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"send_sync".to_vec()),
            OtpErlangTerm::OtpErlangString(name.into()),
            OtpErlangTerm::OtpErlangBinary(request_info),
            OtpErlangTerm::OtpErlangBinary(request.clone()),
            OtpErlangTerm::OtpErlangInteger(timeout as i32),
            OtpErlangTerm::OtpErlangInteger(priority as i32),
        ]))?;
        let _ = self.poll_request(&None, false)?;
        Ok(self.response.take().unwrap())
    }

    /// asynchronous point-multicast communication to services
    pub fn mcast_async(&mut self, name: &str, request: &Vec<u8>,
                       timeout_opt: Option<u32>,
                       request_info_opt: Option<Vec<u8>>,
                       priority_opt: Option<i8>) -> Result<Vec<TransId>> {
        let timeout = timeout_opt.unwrap_or(self.timeout_async);
        let request_info = request_info_opt.unwrap_or(b"".to_vec());
        let priority = priority_opt.unwrap_or(self.priority_default);
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"mcast_async".to_vec()),
            OtpErlangTerm::OtpErlangString(name.into()),
            OtpErlangTerm::OtpErlangBinary(request_info),
            OtpErlangTerm::OtpErlangBinary(request.clone()),
            OtpErlangTerm::OtpErlangInteger(timeout as i32),
            OtpErlangTerm::OtpErlangInteger(priority as i32),
        ]))?;
        let _ = self.poll_request(&None, false)?;
        Ok(self.trans_ids.take().unwrap())
    }

    /// asynchronously receive a response
    pub fn recv_async(&mut self, timeout_opt: Option<u32>,
                      trans_id_opt: Option<TransId>,
                      consume_opt: Option<bool>) ->
                      Result<(Vec<u8>, Vec<u8>, TransId)> {
        let timeout = timeout_opt.unwrap_or(self.timeout_sync);
        let trans_id = trans_id_opt.unwrap_or(TransId::null());
        let consume = consume_opt.unwrap_or(true);
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"recv_async".to_vec()),
            OtpErlangTerm::OtpErlangInteger(timeout as i32),
            OtpErlangTerm::OtpErlangBinary(trans_id.to_vec()),
            OtpErlangTerm::OtpErlangAtomBool(consume),
        ]))?;
        let _ = self.poll_request(&None, false)?;
        Ok(self.response.take().unwrap())
    }

    /// the 0-based index of this process in the service instance
    pub fn process_index(&self) -> u32 {
        self.process_index
    }

    /// the current process count based on the service configuration
    pub fn process_count(&self) -> u32 {
        self.process_count
    }

    /// the count_process_dynamic maximum count based on the
    /// service configuration
    pub fn process_count_max(&self) -> u32 {
        self.process_count_max
    }

    /// the count_process_dynamic minimum count based on the
    /// service configuration
    pub fn process_count_min(&self) -> u32 {
        self.process_count_min
    }

    /// the service name pattern prefix from the service configuration
    pub fn prefix(&self) -> String {
        self.prefix.clone()
    }

    /// the service initialization timeout from the service configuration
    pub fn timeout_initialize(&self) -> u32 {
        self.timeout_initialize
    }

    /// the default asynchronous service request send timeout from
    /// the service configuration
    pub fn timeout_async(&self) -> u32 {
        self.timeout_async
    }

    /// the default synchronous service request send timeout from
    /// the service configuration
    pub fn timeout_sync(&self) -> u32 {
        self.timeout_sync
    }

    /// the service termination timeout based on the service configuration
    pub fn timeout_terminate(&self) -> u32 {
        self.timeout_terminate
    }

    /// the default service request send priority from the
    /// service configuration
    pub fn priority_default(&self) -> i8 {
        self.priority_default
    }

    fn callback(&mut self, request_type: &RequestType,
                name: &str, pattern: &str,
                request_info: &[u8], request: &[u8],
                timeout: u32, priority: i8, trans_id: &[u8],
                source: OtpErlangTerm) -> Result<()> {
        // To avoid overhead and maintain the same CloudI service
        // functionality as the other CloudI API implementations,
        // it is necessary to not support closures with contexts in Rust
        // (std::ops::{Fn, FnMut, FnOnce}).
        // The callback function needs to exist in self.callbacks during the
        // callback execution and self needs to be provided as a
        // mutable reference, so the callback needs to be a
        // function pointer (or a closure without a context).
        let callback_f = match self.callbacks.get_mut(pattern) {
            None => {
                null_response as Callback<'s, S>
            },
            Some(callback_list) => {
                let f = callback_list.pop_front().unwrap();
                callback_list.push_back(f);
                f
            },
        };
        let state_p = self.state as *mut S;
        let callback_result = match callback_f(request_type,
                                               name, pattern,
                                               request_info, request,
                                               timeout, priority,
                                               &TransId::new(trans_id),
                                               &source,
                                               unsafe { &mut *state_p },
                                               self) {
            Response::Response(response) => {
                CallbackResult::ReturnI(b"".to_vec(), response)
            },
            Response::ResponseInfo(response_info, response) => {
                CallbackResult::ReturnI(response_info, response)
            },
            Response::Forward(name_next, response_info, response) => {
                CallbackResult::ForwardI(name_next, response_info, response,
                                         timeout, priority)
            },
            Response::Forward_(name_next, response_info, response,
                               timeout_next, priority_next) => {
                CallbackResult::ForwardI(name_next, response_info, response,
                                         timeout_next, priority_next)
            },
            Response::Null() => {
                CallbackResult::ReturnI(b"".to_vec(), b"".to_vec())
            },
            Response::NullError(error) => {
                eprintln!("{error}");
                CallbackResult::ReturnI(b"".to_vec(), b"".to_vec())
            },
        };
        match callback_result {
            CallbackResult::ReturnI(response_info, response) => {
                let message_type = match request_type {
                    RequestType::ASYNC => b"return_async".to_vec(),
                    RequestType::SYNC => b"return_sync".to_vec(),
                };
                self.send(&OtpErlangTerm::OtpErlangTuple(vec![
                    OtpErlangTerm::OtpErlangAtom(message_type),
                    OtpErlangTerm::OtpErlangString(name.into()),
                    OtpErlangTerm::OtpErlangString(pattern.into()),
                    OtpErlangTerm::OtpErlangBinary(response_info),
                    OtpErlangTerm::OtpErlangBinary(response),
                    OtpErlangTerm::OtpErlangInteger(timeout as i32),
                    OtpErlangTerm::OtpErlangBinary(trans_id.to_vec()),
                    source,
                ]))
            },
            CallbackResult::ForwardI(name_next, request_info_next, request_next,
                                     timeout_next, priority_next) => {
                let message_type = match request_type {
                    RequestType::ASYNC => b"forward_async".to_vec(),
                    RequestType::SYNC => b"forward_sync".to_vec(),
                };
                self.send(&OtpErlangTerm::OtpErlangTuple(vec![
                    OtpErlangTerm::OtpErlangAtom(message_type),
                    OtpErlangTerm::OtpErlangString(name_next.into_bytes()),
                    OtpErlangTerm::OtpErlangBinary(request_info_next),
                    OtpErlangTerm::OtpErlangBinary(request_next),
                    OtpErlangTerm::OtpErlangInteger(timeout_next as i32),
                    OtpErlangTerm::OtpErlangInteger(priority_next as i32),
                    OtpErlangTerm::OtpErlangBinary(trans_id.to_vec()),
                    source,
                ]))
            },
        }
    }

    fn handle_events(&mut self, external: bool, data: &[u8], data_size: usize,
                     i: &mut usize, mut command: u32) -> Result<bool> {
        if command == 0 {
            command = unpack_incoming_u32(i, data)?;
        }
        loop {
            match command {
                MESSAGE_TERM => {
                    self.terminate = true;
                    if external {
                        return Ok(false);
                    }
                    else {
                        let error = ErrorKind::Terminate{
                            timeout: self.timeout_terminate,
                        };
                        return Err(error.into());
                    }
                },
                MESSAGE_REINIT => {
                    self.process_count = unpack_incoming_u32(i, data)?;
                    self.timeout_async = unpack_incoming_u32(i, data)?;
                    self.timeout_sync = unpack_incoming_u32(i, data)?;
                    self.priority_default = unpack_incoming_i8(i, data)?;
                    self.fatal_exceptions = unpack_incoming_bool(i, data)?;
                },
                MESSAGE_KEEPALIVE => {
                    self.send(&OtpErlangTerm::OtpErlangAtomUTF8(
                        b"keepalive".to_vec()))?;
                },
                _ => {
                    return Err(ErrorKind::MessageDecodingError().into());
                },
            }
            if *i == data_size {
                return Ok(true);
            }
            command = unpack_incoming_u32(i, data)?;
        }
    }

    fn poll_request(&mut self, timeout: &Option<Duration>,
                    external: bool) -> Result<bool> {
        if self.terminate {
            if external {
                return Ok(false);
            }
            else {
                let error = ErrorKind::Terminate{
                    timeout: self.timeout_terminate,
                };
                return Err(error.into());
            }
        }
        else if external && ! self.initialization_complete {
            self.send(&OtpErlangTerm::OtpErlangAtomUTF8(b"polling".to_vec()))?;
            self.initialization_complete = true;
        }
        let poll_timer = match timeout {
            Some(_) => Some(Instant::now()),
            None => None,
        };
        let mut buffer = self.recv(timeout)?;
        let mut data_size = buffer.len();
        if data_size == 0 {
            return Ok(true);
        }
        let mut i_value: usize = 0;
        let i = &mut i_value;
        let mut data: &[u8] = buffer.as_slice();
        loop {
            let command = unpack_incoming_u32(i, data)?;
            match command {
                MESSAGE_INIT => {
                    self.process_index = unpack_incoming_u32(i, data)?;
                    self.process_count = unpack_incoming_u32(i, data)?;
                    self.process_count_max = unpack_incoming_u32(i, data)?;
                    self.process_count_min = unpack_incoming_u32(i, data)?;
                    let prefix_size =
                        unpack_incoming_u32(i, data)? as usize;
                    self.prefix = (unpack_incoming_str(i, prefix_size,
                                                       data)?).to_string();
                    self.timeout_initialize = unpack_incoming_u32(i, data)?;
                    self.timeout_async = unpack_incoming_u32(i, data)?;
                    self.timeout_sync = unpack_incoming_u32(i, data)?;
                    self.timeout_terminate = unpack_incoming_u32(i, data)?;
                    self.priority_default = unpack_incoming_i8(i, data)?;
                    self.fatal_exceptions = unpack_incoming_bool(i, data)?;
                    let bind = unpack_incoming_i32(i, data)?;
                    if bind >= 0 {
                        return Err(ErrorKind::InvalidInputError().into());
                    }
                    if *i != data_size {
                        assert_eq!(false, external);
                        let _ = self.handle_events(external, data, data_size,
                                                   i, 0)?;
                    }
                    return Ok(false);
                },
                MESSAGE_SEND_ASYNC |
                MESSAGE_SEND_SYNC => {
                    let request_type = if command == MESSAGE_SEND_ASYNC {
                        RequestType::ASYNC
                    }
                    else { 
                        assert_eq!(MESSAGE_SEND_SYNC, command);
                        RequestType::SYNC
                    };
                    let name_size = unpack_incoming_u32(i, data)? as usize;
                    let name = unpack_incoming_str(i, name_size, data)?;
                    let pattern_size = unpack_incoming_u32(i, data)? as usize;
                    let pattern = unpack_incoming_str(i, pattern_size, data)?;
                    let request_info_size =
                        unpack_incoming_u32(i, data)? as usize;
                    let request_info =
                        unpack_incoming_bytes_1(i, request_info_size, data)?;
                    let request_size = unpack_incoming_u32(i, data)? as usize;
                    let request =
                        unpack_incoming_bytes_1(i, request_size, data)?;
                    let request_timeout = unpack_incoming_u32(i, data)?;
                    let priority = unpack_incoming_i8(i, data)?;
                    let trans_id = unpack_incoming_bytes(i, 16, data)?;
                    let source_size = unpack_incoming_u32(i, data)? as usize;
                    let source = unpack_incoming_pid(i, source_size, data)?;
                    if *i != data_size {
                        assert_eq!(true, external);
                        if ! self.handle_events(external, data, data_size,
                                                i, 0)? {
                            return Ok(false);
                        }
                    }
                    self.callback(&request_type, name, pattern,
                                  request_info, request,
                                  request_timeout, priority,
                                  trans_id, source)?;
                    if self.terminate {
                        return Ok(false);
                    }
                },
                MESSAGE_RECV_ASYNC |
                MESSAGE_RETURN_SYNC => {
                    let response_info_size =
                        unpack_incoming_u32(i, data)? as usize;
                    let response_info =
                        unpack_incoming_bytes_1(i, response_info_size, data)?;
                    let response_size = unpack_incoming_u32(i, data)? as usize;
                    let response =
                        unpack_incoming_bytes_1(i, response_size, data)?;
                    let trans_id = unpack_incoming_bytes(i, 16, data)?;
                    if *i != data_size {
                        assert_eq!(false, external);
                        let _ = self.handle_events(external, data, data_size,
                                                   i, 0)?;
                    }
                    self.response = Some((response_info.to_vec(),
                                          response.to_vec(),
                                          TransId::new(trans_id)));
                    return Ok(false);
                },
                MESSAGE_RETURN_ASYNC => {
                    let trans_id = unpack_incoming_bytes(i, 16, data)?;
                    if *i != data_size {
                        assert_eq!(false, external);
                        let _ = self.handle_events(external, data, data_size,
                                                   i, 0)?;
                    }
                    self.trans_id = Some(TransId::new(trans_id));
                    return Ok(false);
                },
                MESSAGE_RETURNS_ASYNC => {
                    let trans_id_count =
                        unpack_incoming_u32(i, data)? as usize;
                    let mut trans_ids: Vec<TransId> = Vec::new();
                    for _ in 0..trans_id_count {
                        let trans_id = unpack_incoming_bytes(i, 16, data)?;
                        trans_ids.push(TransId::new(trans_id));
                    }
                    self.trans_ids = Some(trans_ids);
                    return Ok(false);
                },
                MESSAGE_SUBSCRIBE_COUNT => {
                    let count = unpack_incoming_u32(i, data)?;
                    self.subscribe_count = Some(count);
                    if *i != data_size {
                        assert_eq!(false, external);
                        let _ = self.handle_events(external, data, data_size,
                                                   i, 0)?;
                    }
                },
                MESSAGE_TERM => {
                    if ! self.handle_events(external, data, data_size,
                                            i, command)? {
                        return Ok(false);
                    }
                    assert!(false);
                },
                MESSAGE_REINIT => {
                    self.process_count = unpack_incoming_u32(i, data)?;
                    self.timeout_async = unpack_incoming_u32(i, data)?;
                    self.timeout_sync = unpack_incoming_u32(i, data)?;
                    self.priority_default = unpack_incoming_i8(i, data)?;
                    self.fatal_exceptions = unpack_incoming_bool(i, data)?;
                    if *i != data_size {
                        continue
                    }
                },
                MESSAGE_KEEPALIVE => {
                    self.send(&OtpErlangTerm::OtpErlangAtomUTF8(
                        b"keepalive".to_vec()))?;
                    if *i != data_size {
                        continue
                    }
                },
                _ => {
                    return Err(ErrorKind::MessageDecodingError().into());
                },
            };
            let timeout_value = match timeout {
                Some(duration) => {
                    let timeout_value_new = 
                        duration.checked_sub(poll_timer.unwrap().elapsed());
                    if timeout_value_new == None {
                        return Ok(true);
                    }
                    timeout_value_new
                },
                None => None,
            };
            buffer = self.recv(&timeout_value)?;
            data_size = buffer.len();
            if data_size == 0 {
                return Ok(true);
            }
            *i = 0;
            data = buffer.as_slice();
        }
    }

    /// blocks to process incoming CloudI service requests
    pub fn poll(&mut self, timeout: i32) -> Result<bool> {
        let timeout_opt: Option<Duration> = if timeout < 0 {
            None
        }
        else if timeout == 0 {
            Some(Duration::from_millis(1))
        }
        else {
            Some(Duration::from_millis(timeout as u64))
        };
        self.poll_request(&timeout_opt, true)
    }

    /// shutdown the service successfully
    pub fn shutdown(&mut self, reason_opt: Option<String>) -> Result<()> {
        let reason = reason_opt.unwrap_or("".to_string());
        self.send(&OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangAtom(b"shutdown".to_vec()),
            OtpErlangTerm::OtpErlangString(reason.into_bytes()),
        ]))
    }

    fn send(&mut self, command: &OtpErlangTerm) -> Result<()> {
        let data = erlang::term_to_binary(command)?;
        if self.use_header {
            let length = data.len() as u32;
            self.send_data(&length.to_be_bytes())?;
        }
        self.send_data(data.as_slice())
    }

    fn send_data(&mut self, data: &[u8]) -> Result<()> {
        match &mut self.socket_kind {
            SocketKind::TCP(socket) => {
                Ok(socket.write_all(data)?)
            },
            SocketKind::UDP(socket) => {
                let _ = socket.send(data)?;
                Ok(())
            },
        }
    }

    fn recv(&mut self, timeout: &Option<Duration>) -> Result<Vec<u8>> {
        let mut size = self.buffer_recv.len();
        assert_eq!(0, size);
        match &mut self.socket_kind {
            SocketKind::TCP(socket) => {
                socket.set_read_timeout(*timeout)?;
                assert_eq!(true, self.use_header);
                let mut header: [u8; 4] = [0; 4];
                socket.read_exact(&mut header)?;
                let length = u32::from_be_bytes(header);
                self.buffer_recv.resize(size + length as usize, 0);
                let mut data: &mut [u8] = &mut self.buffer_recv[size..];
                socket.read_exact(&mut data)?;
            },
            SocketKind::UDP(socket) => {
                socket.set_read_timeout(*timeout)?;
                assert_eq!(false, self.use_header);
                self.buffer_recv.resize(size + self.buffer_size, 0);
                let mut data: &mut [u8] = &mut self.buffer_recv[size..];
                let mut length = socket.recv(&mut data)?;
                while length == self.buffer_size {
                    size += length;
                    self.buffer_recv.resize(size + self.buffer_size, 0);
                    data = &mut self.buffer_recv[size..];
                    length = socket.recv(&mut data)?;
                }
                self.buffer_recv.resize(size + length, 0);
            },
        };
        let buffer = self.buffer_recv.to_vec();
        self.buffer_recv.clear();
        Ok(buffer)
    }
}

/// the 0-based index of this process in the service instance
pub fn process_index() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_PROCESS_INDEX")
}

/// the count_process_dynamic maximum count based on the service configuration
pub fn process_count_max() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_PROCESS_COUNT_MAX")
}

/// the count_process_dynamic minimum count based on the service configuration
pub fn process_count_min() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_PROCESS_COUNT_MIN")
}

/// the service initialization timeout from the service configuration
pub fn timeout_initialize() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_TIMEOUT_INITIALIZE")
}

/// the service termination timeout based on the service configuration
pub fn timeout_terminate() -> Result<u32> {
    getenv_to_u32("CLOUDI_API_INIT_TIMEOUT_TERMINATE")
}

const MESSAGE_INIT: u32 = 1;
const MESSAGE_SEND_ASYNC: u32 = 2;
const MESSAGE_SEND_SYNC: u32 = 3;
const MESSAGE_RECV_ASYNC: u32 = 4;
const MESSAGE_RETURN_ASYNC: u32 = 5;
const MESSAGE_RETURN_SYNC: u32 = 6;
const MESSAGE_RETURNS_ASYNC: u32 = 7;
const MESSAGE_KEEPALIVE: u32 = 8;
const MESSAGE_REINIT: u32 = 9;
const MESSAGE_SUBSCRIBE_COUNT: u32 = 10;
const MESSAGE_TERM: u32 = 11;

enum CallbackResult {
    ReturnI(Vec<u8>, Vec<u8>),
    ForwardI(String, Vec<u8>, Vec<u8>, Timeout, Priority),
}

#[derive(Debug)]
enum SocketKind {
    TCP(TcpStream),
    UDP(UdpSocket),
}

fn null_response<'s, S>(_request_type: &RequestType,
                        _name: &str,
                        _pattern: &str,
                        _request_info: &[u8],
                        _request: &[u8],
                        _timeout: Timeout,
                        _priority: Priority,
                        _trans_id: &TransId,
                        _source: &Source,
                        _state: &mut S,
                        _api: &mut API<'s, S>) -> Response {
    Response::Null()
}

fn getenv_to_u32(name: &'static str) -> Result<u32> {
    match std::env::var(name) {
        Ok(value) => {
            match value.parse::<u32>() {
                Ok(value_u32) => Ok(value_u32),
                Err(_) => Err(ErrorKind::InvalidInputError().into()),
            }
        },
        Err(_) => Err(ErrorKind::InvalidInputError().into()),
    }
}

fn unpack_incoming_str<'s>(i: &mut usize, size: usize,
                           data: &'s [u8]) -> Result<&'s str> {
    let s = unsafe {
        std::str::from_utf8_unchecked(slice_get(data, *i..*i + size - 1)?)
    };
    *i += size;
    Ok(s)
}

fn unpack_incoming_bytes<'s>(i: &mut usize, size: usize,
                             data: &'s [u8]) -> Result<&'s [u8]> {
    let bytes = slice_get(data, *i..*i + size)?;
    *i += size;
    Ok(bytes)
}

fn unpack_incoming_bytes_1<'s>(i: &mut usize, size: usize,
                               data: &'s [u8]) -> Result<&'s [u8]> {
    let bytes = unpack_incoming_bytes(i, size, data)?;
    // skip C string null termination character
    *i += 1;
    Ok(bytes)
}

fn unpack_incoming_pid(i: &mut usize, size: usize,
                       data: &[u8]) -> Result<OtpErlangTerm> {
    let pid = erlang::binary_to_term(slice_get(data, *i..*i + size)?)?;
    *i += size;
    Ok(pid)
}

fn unpack_incoming_u32(i: &mut usize, data: &[u8]) -> Result<u32> {
    let bytes: [u8; 4] = (slice_get(data, *i..*i + 4)?).try_into().unwrap();
    *i += 4;
    Ok(u32::from_ne_bytes(bytes))
}

fn unpack_incoming_i32(i: &mut usize, data: &[u8]) -> Result<i32> {
    let bytes: [u8; 4] = (slice_get(data, *i..*i + 4)?).try_into().unwrap();
    *i += 4;
    Ok(i32::from_ne_bytes(bytes))
}

fn unpack_incoming_i8(i: &mut usize, data: &[u8]) -> Result<i8> {
    let value: i8 = *slice_get(data, *i)? as i8;
    *i += 1;
    Ok(value)
}

fn unpack_incoming_bool(i: &mut usize, data: &[u8]) -> Result<bool> {
    let value = if *slice_get(data, *i)? == 0 { false } else { true };
    *i += 1;
    Ok(value)
}

fn slice_get<I>(data: &[u8], index: I) ->
Result<&<I as std::slice::SliceIndex<[u8]>>::Output>
where I: std::slice::SliceIndex<[u8]>, {
    match data.get(index) {
        Some(result) => Ok(result),
        None => Err(ErrorKind::MessageDecodingError().into()),
    }
}

