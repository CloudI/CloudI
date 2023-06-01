//-*-Mode:rust;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
//ex: set ft=rust fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

//! # Messaging Integration Test with Rust

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

#![crate_name = "messaging_rust"]
#![crate_type = "bin"]

extern crate erlang;
extern crate cloudi;

type StateType = ();

fn task(thread_index: u32) {
    let mut state_value = ();
    let mut api = cloudi::API::new(thread_index, &mut state_value).unwrap();

    api.subscribe("a/b/c/d", sequence1_abcd).unwrap();
    api.subscribe("a/b/c/*", sequence1_abc_).unwrap();
    api.subscribe("a/b/*/d", sequence1_ab_d).unwrap();
    api.subscribe("a/*/c/d", sequence1_a_cd).unwrap();
    api.subscribe("*/b/c/d", sequence1__bcd).unwrap();
    api.subscribe("a/b/*", sequence1_ab__).unwrap();
    api.subscribe("a/*/d", sequence1_a__d).unwrap();
    api.subscribe("*/c/d", sequence1___cd).unwrap();
    api.subscribe("a/*", sequence1_a___).unwrap();
    api.subscribe("*/d", sequence1____d).unwrap();
    api.subscribe("*", sequence1_____).unwrap();
    api.subscribe("sequence1", sequence1).unwrap();
    api.subscribe("e", sequence2_e1).unwrap();
    api.subscribe("e", sequence2_e2).unwrap();
    api.subscribe("e", sequence2_e3).unwrap();
    api.subscribe("e", sequence2_e4).unwrap();
    api.subscribe("e", sequence2_e5).unwrap();
    api.subscribe("e", sequence2_e6).unwrap();
    api.subscribe("e", sequence2_e7).unwrap();
    api.subscribe("e", sequence2_e8).unwrap();
    api.subscribe("sequence2", sequence2).unwrap();
    api.subscribe("f1", sequence3_f1).unwrap();
    api.subscribe("f2", sequence3_f2).unwrap();
    api.subscribe("g1", sequence3_g1).unwrap();
    api.subscribe("sequence3", sequence3).unwrap();
    if thread_index == 0 {
        let _ = api.send_async((api.prefix() + "sequence1").as_str(),
                               &b"1".to_vec(),
                               None, None, None).unwrap();
    }
    match api.poll(-1) {
        Ok(result) => {
            assert_eq!(false, result);
            println!("terminate messaging rust");
        },
        Err(error) => {
            eprintln!("{:#?}", error);
            std::process::abort();
        },
    }
}

fn sequence1_abcd(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/b/c/d");
    assert!(request == b"test1");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_abc_(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/b/c/*");
    assert!(request == b"test2" || request == b"test3");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_ab_d(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/b/*/d");
    assert!(request == b"test4" || request == b"test5");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_a_cd(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/*/c/d");
    assert!(request == b"test6" || request == b"test7");
    cloudi::Response::Response(request.to_vec())
}

#[allow(non_snake_case)]
fn sequence1__bcd(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "*/b/c/d");
    assert!(request == b"test8" || request == b"test9");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_ab__(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/b/*");
    assert!(request == b"test10");
    cloudi::Response::Response(request.to_vec())
}

#[allow(non_snake_case)]
fn sequence1_a__d(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/*/d");
    assert!(request == b"test11");
    cloudi::Response::Response(request.to_vec())
}

#[allow(non_snake_case)]
fn sequence1___cd(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "*/c/d");
    assert!(request == b"test12");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_a___(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "a/*");
    assert!(request == b"test13");
    cloudi::Response::Response(request.to_vec())
}

#[allow(non_snake_case)]
fn sequence1____d(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "*/d");
    assert!(request == b"test14");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1_____(_request_type: &cloudi::RequestType,
                  _name: &str,
                  pattern: &str,
                  _request_info: &[u8],
                  request: &[u8],
                  _timeout: cloudi::Timeout,
                  _priority: cloudi::Priority,
                  _trans_id: &cloudi::TransId,
                  _source: &cloudi::Source,
                  _state: &mut StateType,
                  api: &mut cloudi::API<StateType>) -> cloudi::Response {
    assert_eq!(pattern.to_string(), api.prefix() + "*");
    assert!(request == b"test15");
    cloudi::Response::Response(request.to_vec())
}

fn sequence1(_request_type: &cloudi::RequestType,
             _name: &str,
             _pattern: &str,
             _request_info: &[u8],
             request: &[u8],
             _timeout: cloudi::Timeout,
             _priority: cloudi::Priority,
             _trans_id: &cloudi::TransId,
             _source: &cloudi::Source,
             _state: &mut StateType,
             api: &mut cloudi::API<StateType>) -> cloudi::Response {
    // consume all the 'end' responses from all sequences handled
    // by this service
    while api.recv_async(Some(1000), None, None).unwrap().1 == b"end".to_vec() {
    }
    let iteration = request_as_usize(request);
    println!("messaging sequence1 start rust ({iteration})");
    let test1_id = api.send_async((api.prefix() + "a/b/c/d").as_str(),
                                  &b"test1".to_vec(),
                                  None, None, None).unwrap();
    let test2_id = api.send_async((api.prefix() + "a/b/c/z").as_str(),
                                  &b"test2".to_vec(),
                                  None, None, None).unwrap();
    let test3_id = api.send_async((api.prefix() + "a/b/c/dd").as_str(),
                                  &b"test3".to_vec(),
                                  None, None, None).unwrap();
    let test4_id = api.send_async((api.prefix() + "a/b/z/d").as_str(),
                                  &b"test4".to_vec(),
                                  None, None, None).unwrap();
    let test5_id = api.send_async((api.prefix() + "a/b/cc/d").as_str(),
                                  &b"test5".to_vec(),
                                  None, None, None).unwrap();
    let test6_id = api.send_async((api.prefix() + "a/z/c/d").as_str(),
                                  &b"test6".to_vec(),
                                  None, None, None).unwrap();
    let test7_id = api.send_async((api.prefix() + "a/bb/c/d").as_str(),
                                  &b"test7".to_vec(),
                                  None, None, None).unwrap();
    let test8_id = api.send_async((api.prefix() + "z/b/c/d").as_str(),
                                  &b"test8".to_vec(),
                                  None, None, None).unwrap();
    let test9_id = api.send_async((api.prefix() + "aa/b/c/d").as_str(),
                                  &b"test9".to_vec(),
                                  None, None, None).unwrap();
    let test10_id = api.send_async((api.prefix() + "a/b/czd").as_str(),
                                   &b"test10".to_vec(),
                                   None, None, None).unwrap();
    let test11_id = api.send_async((api.prefix() + "a/bzc/d").as_str(),
                                   &b"test11".to_vec(),
                                   None, None, None).unwrap();
    let test12_id = api.send_async((api.prefix() + "azb/c/d").as_str(),
                                   &b"test12".to_vec(),
                                   None, None, None).unwrap();
    let test13_id = api.send_async((api.prefix() + "a/bzczd").as_str(),
                                   &b"test13".to_vec(),
                                   None, None, None).unwrap();
    let test14_id = api.send_async((api.prefix() + "azbzc/d").as_str(),
                                   &b"test14".to_vec(),
                                   None, None, None).unwrap();
    let test15_id = api.send_async((api.prefix() + "azbzczd").as_str(),
                                   &b"test15".to_vec(),
                                   None, None, None).unwrap();
    // n.b., depends on cloudi_core_i_constants.hrl having
    // RECV_ASYNC_STRATEGY == recv_async_select_oldest
    let _ = api.recv_async(None, Some(test1_id), Some(false)).unwrap();
    let (_, test1_check,
         test1_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test1".to_vec(), test1_check);
    assert_eq!(test1_id, test1_id_check);
    let _ = api.recv_async(None, Some(test2_id), Some(false)).unwrap();
    let (_, test2_check,
         test2_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test2".to_vec(), test2_check);
    assert_eq!(test2_id, test2_id_check);
    let _ = api.recv_async(None, Some(test3_id), Some(false)).unwrap();
    let (_, test3_check,
         test3_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test3".to_vec(), test3_check);
    assert_eq!(test3_id, test3_id_check);
    let _ = api.recv_async(None, Some(test4_id), Some(false)).unwrap();
    let (_, test4_check,
         test4_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test4".to_vec(), test4_check);
    assert_eq!(test4_id, test4_id_check);
    let _ = api.recv_async(None, Some(test5_id), Some(false)).unwrap();
    let (_, test5_check,
         test5_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test5".to_vec(), test5_check);
    assert_eq!(test5_id, test5_id_check);
    let _ = api.recv_async(None, Some(test6_id), Some(false)).unwrap();
    let (_, test6_check,
         test6_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test6".to_vec(), test6_check);
    assert_eq!(test6_id, test6_id_check);
    let _ = api.recv_async(None, Some(test7_id), Some(false)).unwrap();
    let (_, test7_check,
         test7_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test7".to_vec(), test7_check);
    assert_eq!(test7_id, test7_id_check);
    let _ = api.recv_async(None, Some(test8_id), Some(false)).unwrap();
    let (_, test8_check,
         test8_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test8".to_vec(), test8_check);
    assert_eq!(test8_id, test8_id_check);
    let _ = api.recv_async(None, Some(test9_id), Some(false)).unwrap();
    let (_, test9_check,
         test9_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test9".to_vec(), test9_check);
    assert_eq!(test9_id, test9_id_check);
    let _ = api.recv_async(None, Some(test10_id), Some(false)).unwrap();
    let (_, test10_check,
         test10_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test10".to_vec(), test10_check);
    assert_eq!(test10_id, test10_id_check);
    let _ = api.recv_async(None, Some(test11_id), Some(false)).unwrap();
    let (_, test11_check,
         test11_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test11".to_vec(), test11_check);
    assert_eq!(test11_id, test11_id_check);
    let _ = api.recv_async(None, Some(test12_id), Some(false)).unwrap();
    let (_, test12_check,
         test12_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test12".to_vec(), test12_check);
    assert_eq!(test12_id, test12_id_check);
    let _ = api.recv_async(None, Some(test13_id), Some(false)).unwrap();
    let (_, test13_check,
         test13_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test13".to_vec(), test13_check);
    assert_eq!(test13_id, test13_id_check);
    let _ = api.recv_async(None, Some(test14_id), Some(false)).unwrap();
    let (_, test14_check,
         test14_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test14".to_vec(), test14_check);
    assert_eq!(test14_id, test14_id_check);
    let _ = api.recv_async(None, Some(test15_id), Some(false)).unwrap();
    let (_, test15_check,
         test15_id_check) = api.recv_async(None, None, None).unwrap();
    assert_eq!(b"test15".to_vec(), test15_check);
    assert_eq!(test15_id, test15_id_check);
    println!("messaging sequence1 end rust ({iteration})");
    // start sequence2
    let _ = api.send_async((api.prefix() + "sequence2").as_str(),
                           &request.to_vec(), None, None, None).unwrap();
    cloudi::Response::Response(b"end".to_vec())
}

fn sequence2_e1(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"1".to_vec())
}

fn sequence2_e2(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"2".to_vec())
}

fn sequence2_e3(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"3".to_vec())
}

fn sequence2_e4(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"4".to_vec())
}

fn sequence2_e5(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"5".to_vec())
}

fn sequence2_e6(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"6".to_vec())
}

fn sequence2_e7(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"7".to_vec())
}

fn sequence2_e8(_request_type: &cloudi::RequestType,
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
    cloudi::Response::Response(b"8".to_vec())
}

fn sequence2(_request_type: &cloudi::RequestType,
             _name: &str,
             _pattern: &str,
             _request_info: &[u8],
             request: &[u8],
             _timeout: cloudi::Timeout,
             _priority: cloudi::Priority,
             _trans_id: &cloudi::TransId,
             _source: &cloudi::Source,
             _state: &mut StateType,
             api: &mut cloudi::API<StateType>) -> cloudi::Response {
    let iteration = request_as_usize(request);
    println!("messaging sequence2 start rust ({iteration})");
    loop {
        // the sending process is excluded from the services that receive
        // the asynchronous message, so in this case, the receiving thread
        // will not be called, despite the fact it has subscribed to 'e',
        // to prevent a process (in this case thread) from deadlocking
        // with itself.
        let e_ids = api.mcast_async((api.prefix() + "e").as_str(),
                                    &b" ".to_vec(), None, None, None).unwrap();
        // 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
        // since 1 thread is sending the mcast_async, so 3 * 8 == 24
        if e_ids.len() == 24 {
            let mut e_check_list = Vec::with_capacity(24);
            for e_id in e_ids.iter() {
                let (_, e_check,
                     e_id_check) = api.recv_async(None, Some(*e_id),
                                                  None).unwrap();
                assert_eq!(*e_id, e_id_check);
                e_check_list.extend_from_slice(&e_check[..1]);
            }
            e_check_list.sort();
            assert_eq!(b"111222333444555666777888", e_check_list.as_slice());
            break;
        }
        else {
            println!("Waiting for {} services to initialize",
                     4.0 - (e_ids.len() as f64) / 8.0);
            for e_id in e_ids.iter() {
                let (_, _, e_id_check) = api.recv_async(None, Some(*e_id),
                                                        None).unwrap();
                assert_eq!(*e_id, e_id_check);
            }
            let (_, _, null_id) = api.recv_async(Some(1000),
                                                 None, None).unwrap();
            assert!(null_id.is_timeout());
        }
    }
    println!("messaging sequence2 end rust ({iteration})");
    // start sequence3
    let _ = api.send_async((api.prefix() + "sequence3").as_str(),
                           &request.to_vec(), None, None, None).unwrap();
    cloudi::Response::Response(b"end".to_vec())
}

fn sequence3_f1(_request_type: &cloudi::RequestType,
                _name: &str,
                _pattern: &str,
                _request_info: &[u8],
                request: &[u8],
                _timeout: cloudi::Timeout,
                _priority: cloudi::Priority,
                _trans_id: &cloudi::TransId,
                _source: &cloudi::Source,
                _state: &mut StateType,
                api: &mut cloudi::API<StateType>) -> cloudi::Response {
    match request_as_usize(request) {
        4 => {
            cloudi::Response::Response(b"done".to_vec())
        },
        request_i => {
            let request_new = request_i + 2; // two steps forward
            cloudi::Response::Forward(api.prefix() + "f2", b"".to_vec(),
                                      format!("{request_new}").into())
        },
    }
}

fn sequence3_f2(_request_type: &cloudi::RequestType,
                _name: &str,
                _pattern: &str,
                _request_info: &[u8],
                request: &[u8],
                _timeout: cloudi::Timeout,
                _priority: cloudi::Priority,
                _trans_id: &cloudi::TransId,
                _source: &cloudi::Source,
                _state: &mut StateType,
                api: &mut cloudi::API<StateType>) -> cloudi::Response {
    let request_i = request_as_usize(request);
    let request_new = request_i - 1; // one step back
    cloudi::Response::Forward(api.prefix() + "f1", b"".to_vec(),
                              format!("{request_new}").into())
}

fn sequence3_g1(_request_type: &cloudi::RequestType,
                _name: &str,
                _pattern: &str,
                _request_info: &[u8],
                request: &[u8],
                _timeout: cloudi::Timeout,
                _priority: cloudi::Priority,
                _trans_id: &cloudi::TransId,
                _source: &cloudi::Source,
                _state: &mut StateType,
                _api: &mut cloudi::API<StateType>) -> cloudi::Response {
    cloudi::Response::Response([request, b"suffix"].concat())
}

fn sequence3(_request_type: &cloudi::RequestType,
             _name: &str,
             _pattern: &str,
             _request_info: &[u8],
             request: &[u8],
             _timeout: cloudi::Timeout,
             _priority: cloudi::Priority,
             _trans_id: &cloudi::TransId,
             _source: &cloudi::Source,
             _state: &mut StateType,
             api: &mut cloudi::API<StateType>) -> cloudi::Response {
    let mut iteration = request_as_usize(request);
    println!("messaging sequence3 start rust ({iteration})");
    let test1_id = api.send_async((api.prefix() + "f1").as_str(),
                                  &b"0".to_vec(), None, None, None).unwrap();
    let (_, test1_check,
         test1_id_check) = api.recv_async(None, Some(test1_id), None).unwrap();
    assert_eq!(test1_id, test1_id_check);
    assert_eq!(b"done".to_vec(), test1_check);
    let (_, test2_check, _) = api.send_sync((api.prefix() + "g1").as_str(),
                                            &b"prefix_".to_vec(),
                                            None, None, None).unwrap();
    assert_eq!(b"prefix_suffix".to_vec(), test2_check);
    println!("messaging sequence3 end rust ({iteration})");
    // loop to find any infrequent problems, restart sequence1
    iteration += 1;
    let _ = api.send_async((api.prefix() + "sequence1").as_str(),
                           &format!("{iteration}").into(),
                           None, None, None).unwrap();
    cloudi::Response::Response(b"end".to_vec())
}

fn request_as_usize(request: &[u8]) -> usize {
    let iteration_str = unsafe {
        std::str::from_utf8_unchecked(request)
    };
    iteration_str.parse::<usize>().unwrap()
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
