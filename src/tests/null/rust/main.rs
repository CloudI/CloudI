//-*-Mode:rust;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
//ex: set ft=rust fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

//! # Null Integration Test with Rust

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

#![crate_name = "null_rust"]
#![crate_type = "bin"]

extern crate erlang;
extern crate cloudi;

type StateType = ();

fn request(_request_type: &cloudi::RequestType,
           _name: &str,
           _pattern: &str,
           _request_info: &[u8],
           _request: &[u8],
           _timeout: cloudi::Timeout,
           _priority: cloudi::Priority,
           _trans_id: &cloudi::TransId,
           _source: &cloudi::Source,
           _state: &mut StateType,
           _api: &mut cloudi::API<StateType>) -> cloudi::Response {
    println!("null rust");
    cloudi::Response::Null()
}

fn task(thread_index: u32) {
    let mut state_value = ();
    let mut api = cloudi::API::new(thread_index, &mut state_value).unwrap();

    api.subscribe("rust/get", request).unwrap();
    match api.poll(-1) {
        Ok(result) => {
            assert_eq!(false, result);
            println!("terminate null rust");
        },
        Err(error) => {
            eprintln!("{:#?}", error);
            std::process::abort();
        },
    }
}

fn main() {
    let thread_count: u32 = cloudi::thread_count().unwrap();
    std::thread::scope(move |s| {
        for thread_index in 0..thread_count {
            s.spawn(move || {
                task(thread_index)
            });
        }
    });
}
