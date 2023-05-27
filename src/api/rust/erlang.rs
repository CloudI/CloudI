//-*-Mode:rust;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
//ex: set ft=rust fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

//! # Erlang External Term Format Encoding/Decoding
//!
//! Provides all encoding and decoding for the Erlang External Term Format
//! (as defined at [https://erlang.org/doc/apps/erts/erl_ext_dist.html](https://erlang.org/doc/apps/erts/erl_ext_dist.html)).

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

#![crate_name = "erlang"]
#![crate_type = "lib"]

use std::collections::BTreeMap;

pub type Result<T> = std::result::Result<T, Error>;

// tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
const TAG_VERSION: u8 = 131;
const TAG_COMPRESSED_ZLIB: u8 = 80;
const TAG_NEW_FLOAT_EXT: u8 = 70;
const TAG_BIT_BINARY_EXT: u8 = 77;
const TAG_ATOM_CACHE_REF: u8 = 78;
const TAG_NEW_PID_EXT: u8 = 88;
const TAG_NEW_PORT_EXT: u8 = 89;
const TAG_NEWER_REFERENCE_EXT: u8 = 90;
const TAG_SMALL_INTEGER_EXT: u8 = 97;
const TAG_INTEGER_EXT: u8 = 98;
const TAG_FLOAT_EXT: u8 = 99;
const TAG_ATOM_EXT: u8 = 100;
const TAG_REFERENCE_EXT: u8 = 101;
const TAG_PORT_EXT: u8 = 102;
const TAG_PID_EXT: u8 = 103;
const TAG_SMALL_TUPLE_EXT: u8 = 104;
const TAG_LARGE_TUPLE_EXT: u8 = 105;
const TAG_NIL_EXT: u8 = 106;
const TAG_STRING_EXT: u8 = 107;
const TAG_LIST_EXT: u8 = 108;
const TAG_BINARY_EXT: u8 = 109;
const TAG_SMALL_BIG_EXT: u8 = 110;
const TAG_LARGE_BIG_EXT: u8 = 111;
const TAG_NEW_FUN_EXT: u8 = 112;
const TAG_EXPORT_EXT: u8 = 113;
const TAG_NEW_REFERENCE_EXT: u8 = 114;
const TAG_SMALL_ATOM_EXT: u8 = 115;
const TAG_MAP_EXT: u8 = 116;
const TAG_FUN_EXT: u8 = 117;
const TAG_ATOM_UTF8_EXT: u8 = 118;
const TAG_SMALL_ATOM_UTF8_EXT: u8 = 119;
const TAG_V4_PORT_EXT: u8 = 120;
const TAG_LOCAL_EXT: u8 = 121;

/// f64 comparable data for [`OtpErlangTerm::OtpErlangFloat`]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Float {
    bits: u64,
}

impl Float {
    pub fn value(&self) -> f64 {
        f64::from_bits(self.bits)
    }
}

impl From<f64> for Float {
    fn from(x: f64) -> Self {
        Float {
            bits: x.to_bits(),
        }
    }
}

/// Pid data for [`OtpErlangTerm::OtpErlangPid`]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Pid {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    serial: Vec<u8>,
    creation: Vec<u8>,
}

/// Port data for [`OtpErlangTerm::OtpErlangPort`]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Port {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    creation: Vec<u8>,
}

/// Reference data for [`OtpErlangTerm::OtpErlangReference`]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Reference {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    creation: Vec<u8>,
}

/// Function data for [`OtpErlangTerm::OtpErlangFunction`]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Function {
    tag: u8,
    value: Vec<u8>,
}

/// Erlang term representation
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum OtpErlangTerm {
    OtpErlangInteger(i32),
    OtpErlangFloat(Float),
    OtpErlangAtom(Vec<u8>),
    OtpErlangAtomUTF8(Vec<u8>),
    OtpErlangAtomCacheRef(u8),
    OtpErlangAtomBool(bool),
    OtpErlangString(Vec<u8>),
    OtpErlangBinary(Vec<u8>),
    OtpErlangBinaryBits(Vec<u8>, u8),
    OtpErlangList(Vec<OtpErlangTerm>),
    OtpErlangListImproper(Vec<OtpErlangTerm>),
    OtpErlangTuple(Vec<OtpErlangTerm>),
    OtpErlangMap(BTreeMap<OtpErlangTerm, OtpErlangTerm>),
    OtpErlangPid(Pid),
    OtpErlangPort(Port),
    OtpErlangReference(Reference),
    OtpErlangFunction(Function),
}

/// Error description
#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
    OutputError(&'static str),
    ParseError(&'static str),
    UnexpectedError(),
}

/// Error data
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl Error {
    fn new<E>(error: E) -> Self
    where E: Into<Box<dyn std::error::Error + Send + Sync>>, {
        Error {
            kind: ErrorKind::UnexpectedError(),
            source: Some(error.into()),
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
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(error: std::num::ParseFloatError) -> Self {
        Error::new(error)
    }
}

fn slice_get<I>(data: &[u8], index: I) ->
Result<&<I as std::slice::SliceIndex<[u8]>>::Output>
where I: std::slice::SliceIndex<[u8]>, {
    match data.get(index) {
        Some(result) => Ok(result),
        None => Err(ErrorKind::ParseError("missing data").into()),
    }
}

fn pack_u16(value: u16, data: &mut Vec<u8>) {
    data.extend_from_slice(&value.to_be_bytes());
}

fn pack_u32(value: u32, data: &mut Vec<u8>) {
    data.extend_from_slice(&value.to_be_bytes());
}

fn pack_u64(value: u64, data: &mut Vec<u8>) {
    data.extend_from_slice(&value.to_be_bytes());
}

fn unpack_u16(i: &mut usize, data: &[u8]) -> Result<u16> {
    let bytes: [u8; 2] = (slice_get(data, *i..*i + 2)?).try_into().unwrap();
    *i += 2;
    Ok(u16::from_be_bytes(bytes))
}

fn unpack_u32(i: &mut usize, data: &[u8]) -> Result<u32> {
    let bytes: [u8; 4] = (slice_get(data, *i..*i + 4)?).try_into().unwrap();
    *i += 4;
    Ok(u32::from_be_bytes(bytes))
}

fn unpack_f64(i: &mut usize, data: &[u8]) -> Result<f64> {
    let bytes: [u8; 8] = (slice_get(data, *i..*i + 8)?).try_into().unwrap();
    *i += 8;
    Ok(f64::from_bits(u64::from_be_bytes(bytes)))
}

/// decodes the Erlang External Term Format into [`OtpErlangTerm`]
pub fn binary_to_term(data: &[u8]) -> Result<OtpErlangTerm> {
    let size = data.len();
    if size <= 1 {
        return Err(ErrorKind::ParseError("null input").into());
    }
    if data[0] != TAG_VERSION {
        return Err(ErrorKind::ParseError("invalid version").into());
    }
    let mut i: usize = 1;
    let term = binary_to_term_(&mut i, data)?;
    if i == size {
        Ok(term)
    }
    else {
        Err(ErrorKind::ParseError("unparsed data").into())
    }
}

/// encodes [`OtpErlangTerm`] into the Erlang External Term Format
pub fn term_to_binary(term: &OtpErlangTerm) -> Result<Vec<u8>> {
    let mut data: Vec<u8> = Vec::new();
    data.push(TAG_VERSION);
    let () = term_to_binary_(term, &mut data)?;
    Ok(data)
}

fn binary_to_term_(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_NEW_FLOAT_EXT => {
            let float = unpack_f64(i, data)?;
            Ok(OtpErlangTerm::OtpErlangFloat(float.into()))
        },
        TAG_BIT_BINARY_EXT => {
            let j = unpack_u32(i, data)? as usize;
            let bits = *slice_get(data, *i)?;
            *i += 1;
            let binary = slice_get(data, *i..*i + j)?;
            *i += j;
            if bits == 8 {
                Ok(OtpErlangTerm::OtpErlangBinary(binary.to_vec()))
            }
            else {
                Ok(OtpErlangTerm::OtpErlangBinaryBits(binary.to_vec(), bits))
            }
        },
        TAG_ATOM_CACHE_REF => {
            let atom = *slice_get(data, *i)?;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangAtomCacheRef(atom))
        },
        TAG_SMALL_INTEGER_EXT => {
            let integer = *slice_get(data, *i)? as i32;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_INTEGER_EXT => {
            let integer = unpack_u32(i, data)? as i32;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_FLOAT_EXT => {
            let float_bytes = slice_get(data, *i..*i + 31)?;
            *i += 31;
            let float_str = unsafe {
                std::str::from_utf8_unchecked(float_bytes)
            };
            let float = float_str.parse::<f64>()?;
            Ok(OtpErlangTerm::OtpErlangFloat(float.into()))
        },
        TAG_V4_PORT_EXT |
        TAG_NEW_PORT_EXT |
        TAG_REFERENCE_EXT |
        TAG_PORT_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id_size = match tag {
                TAG_V4_PORT_EXT => 8,
                _ => 4,
            };
            let id = slice_get(data, *i..*i + id_size)?;
            *i += id_size;
            let creation_size = match tag {
                TAG_V4_PORT_EXT | TAG_NEW_PORT_EXT => 4,
                _ => 1,
            };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            if tag == TAG_REFERENCE_EXT {
                Ok(OtpErlangTerm::OtpErlangReference(Reference {
                    node_tag,
                    node,
                    id: id.to_vec(),
                    creation: creation.to_vec(),
                }))
            }
            else {
                Ok(OtpErlangTerm::OtpErlangPort(Port {
                    node_tag,
                    node,
                    id: id.to_vec(),
                    creation: creation.to_vec(),
                }))
            }
        },
        TAG_NEW_PID_EXT |
        TAG_PID_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let serial = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let creation_size = if tag == TAG_NEW_PID_EXT { 4 } else { 1 };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            Ok(OtpErlangTerm::OtpErlangPid(Pid {
                node_tag,
                node,
                id: id.to_vec(),
                serial: serial.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        TAG_SMALL_TUPLE_EXT => {
            let length = *slice_get(data, *i)? as usize;
            *i += 1;
            let tmp = binary_to_term_sequence(i, length, data)?;
            Ok(OtpErlangTerm::OtpErlangTuple(tmp))
        },
        TAG_LARGE_TUPLE_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let tmp = binary_to_term_sequence(i, length, data)?;
            Ok(OtpErlangTerm::OtpErlangTuple(tmp))
        },
        TAG_NIL_EXT => {
            Ok(OtpErlangTerm::OtpErlangList(Vec::new()))
        },
        TAG_STRING_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let string = slice_get(data, *i..*i + j)?;
            *i += j;
            Ok(OtpErlangTerm::OtpErlangString(string.to_vec()))
        },
        TAG_LIST_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let mut tmp = binary_to_term_sequence(i, length, data)?;
            match binary_to_term_(i, data)? {
                OtpErlangTerm::OtpErlangList(v) if v.is_empty() => {
                    Ok(OtpErlangTerm::OtpErlangList(tmp))
                },
                tail => {
                    tmp.push(tail);
                    Ok(OtpErlangTerm::OtpErlangListImproper(tmp))
                },
            }
        },
        TAG_BINARY_EXT => {
            let j = unpack_u32(i, data)? as usize;
            let binary = slice_get(data, *i..*i + j)?;
            *i += j;
            Ok(OtpErlangTerm::OtpErlangBinary(binary.to_vec()))
        },
        TAG_SMALL_BIG_EXT |
        TAG_LARGE_BIG_EXT => {
            Err(ErrorKind::ParseError("rust doesn't provide bigint").into())
        },
        TAG_NEW_FUN_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let value = slice_get(data, *i..*i + length)?;
            *i += length;
            Ok(OtpErlangTerm::OtpErlangFunction(Function {
                tag,
                value: value.to_vec(),
            }))
        },
        TAG_EXPORT_EXT => {
            let i0 = *i;
            let _ = binary_to_atom(i, data)?;
            let _ = binary_to_atom(i, data)?;
            if *slice_get(data, *i)? != TAG_SMALL_INTEGER_EXT {
                Err(ErrorKind::ParseError("invalid small integer tag").into())
            }
            else {
                *i += 2;
                let value = slice_get(data, i0..*i)?;
                Ok(OtpErlangTerm::OtpErlangFunction(Function {
                    tag,
                    value: value.to_vec(),
                }))
            }
        },
        TAG_NEWER_REFERENCE_EXT |
        TAG_NEW_REFERENCE_EXT => {
            let j = (unpack_u16(i, data)? as usize) * 4;
            let (node_tag, node) = binary_to_atom(i, data)?;
            let creation_size = match tag {
                TAG_NEWER_REFERENCE_EXT => 4,
                _ => 1,
            };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            let id = slice_get(data, *i..*i + j)?;
            *i += j;
            Ok(OtpErlangTerm::OtpErlangReference(Reference {
                node_tag,
                node,
                id: id.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        TAG_MAP_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let mut pairs = BTreeMap::new();
            for _ in 0..length {
                let key = binary_to_term_(i, data)?;
                let value = binary_to_term_(i, data)?;
                pairs.insert(key, value);
            }
            Ok(OtpErlangTerm::OtpErlangMap(pairs))
        },
        TAG_FUN_EXT => {
            let i0 = *i;
            let numfree = unpack_u32(i, data)? as usize;
            let _ = binary_to_pid(i, data)?; // pid
            let _ = binary_to_atom(i, data)?; // module
            let _ = binary_to_integer(i, data)?; // index
            let _ = binary_to_integer(i, data)?; // uniq
            let _ = binary_to_term_sequence(i, numfree, data)?; // free
            let value = slice_get(data, i0..*i)?;
            Ok(OtpErlangTerm::OtpErlangFunction(Function {
                tag,
                value: value.to_vec(),
            }))
        },
        TAG_ATOM_UTF8_EXT |
        TAG_ATOM_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let atom_name = slice_get(data, *i..*i + j)?;
            *i += j;
            match atom_name {
                b"true" => Ok(OtpErlangTerm::OtpErlangAtomBool(true)),
                b"false" => Ok(OtpErlangTerm::OtpErlangAtomBool(false)),
                _ if tag == TAG_ATOM_UTF8_EXT => {
                    Ok(OtpErlangTerm::OtpErlangAtomUTF8(atom_name.to_vec()))
                },
                _ => Ok(OtpErlangTerm::OtpErlangAtom(atom_name.to_vec())),
            }
        },
        TAG_SMALL_ATOM_UTF8_EXT |
        TAG_SMALL_ATOM_EXT => {
            let j = *slice_get(data, *i)? as usize;
            *i += 1;
            let atom_name = slice_get(data, *i..*i + j)?;
            *i += j;
            match atom_name {
                b"true" => Ok(OtpErlangTerm::OtpErlangAtomBool(true)),
                b"false" => Ok(OtpErlangTerm::OtpErlangAtomBool(false)),
                _ if tag == TAG_SMALL_ATOM_UTF8_EXT => {
                    Ok(OtpErlangTerm::OtpErlangAtomUTF8(atom_name.to_vec()))
                },
                _ => Ok(OtpErlangTerm::OtpErlangAtom(atom_name.to_vec())),
            }
        },
        TAG_COMPRESSED_ZLIB => {
            Err(ErrorKind::ParseError("rust doesn't provide zlib").into())
        },
        TAG_LOCAL_EXT => {
            Err(ErrorKind::ParseError("LOCAL_EXT is opaque").into())
        },
        _ => Err(ErrorKind::ParseError("invalid tag").into()),
    }
}

fn binary_to_term_sequence(i: &mut usize, length: usize,
                           data: &[u8]) -> Result<Vec<OtpErlangTerm>> {
    let mut sequence: Vec<OtpErlangTerm> = Vec::new();
    for _ in 0..length {
        sequence.push(binary_to_term_(i, data)?);
    }
    Ok(sequence)
}

fn binary_to_integer(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_SMALL_INTEGER_EXT => {
            let integer = *slice_get(data, *i)? as i32;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_INTEGER_EXT => {
            let integer = unpack_u32(i, data)? as i32;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        _ => Err(ErrorKind::ParseError("invalid integer tag").into()),
    }
}

fn binary_to_pid(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_NEW_PID_EXT |
        TAG_PID_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let serial = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let creation_size = if tag == TAG_NEW_PID_EXT { 4 } else { 1 };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            Ok(OtpErlangTerm::OtpErlangPid(Pid {
                node_tag,
                node,
                id: id.to_vec(),
                serial: serial.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        _ => Err(ErrorKind::ParseError("invalid pid tag").into()),
    }
}

fn binary_to_atom(i: &mut usize, data: &[u8]) -> Result<(u8, Vec<u8>)> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_ATOM_CACHE_REF => {
            let value = slice_get(data, *i..*i + 1)?;
            *i += 1;
            Ok((tag, value.to_vec()))
        },
        TAG_ATOM_UTF8_EXT |
        TAG_ATOM_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let value = slice_get(data, *i - 2..*i + j)?;
            *i += j;
            Ok((tag, value.to_vec()))
        },
        TAG_SMALL_ATOM_UTF8_EXT |
        TAG_SMALL_ATOM_EXT => {
            let j = *slice_get(data, *i)? as usize;
            let value = slice_get(data, *i..*i + 1 + j)?;
            *i += 1 + j;
            Ok((tag, value.to_vec()))
        },
        _ => Err(ErrorKind::ParseError("invalid atom tag").into()),
    }
}

fn term_to_binary_(term: &OtpErlangTerm,
                   data: &mut Vec<u8>) -> Result<()> {
    match term {
        OtpErlangTerm::OtpErlangInteger(value) => {
            integer_to_binary(*value, data)
        },
        OtpErlangTerm::OtpErlangFloat(value) => {
            data.push(TAG_NEW_FLOAT_EXT);
            pack_u64(value.bits, data);
            Ok(())
        },
        OtpErlangTerm::OtpErlangAtom(value) => {
            atom_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangAtomUTF8(value) => {
            atom_utf8_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangAtomCacheRef(value) => {
            data.push(TAG_ATOM_CACHE_REF);
            data.push(*value);
            Ok(())
        },
        OtpErlangTerm::OtpErlangAtomBool(value) => {
            if *value {
                atom_utf8_to_binary(&b"true".to_vec(), data)
            }
            else {
                atom_utf8_to_binary(&b"false".to_vec(), data)
            }
        },
        OtpErlangTerm::OtpErlangString(value) => {
            string_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangBinary(value) => {
            binary_bits_to_binary(value, 8, data)
        },
        OtpErlangTerm::OtpErlangBinaryBits(value, bits) => {
            binary_bits_to_binary(value, *bits, data)
        },
        OtpErlangTerm::OtpErlangList(value) => {
            list_to_binary(value, false, data)
        },
        OtpErlangTerm::OtpErlangListImproper(value) => {
            list_to_binary(value, true, data)
        },
        OtpErlangTerm::OtpErlangTuple(value) => {
            tuple_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangMap(value) => {
            map_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangPid(value) => {
            pid_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangPort(value) => {
            port_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangReference(value) => {
            reference_to_binary(value, data)
        },
        OtpErlangTerm::OtpErlangFunction(value) => {
            function_to_binary(value, data)
        },
    }
}

fn string_to_binary(value: &Vec<u8>, data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if length == 0 {
        data.push(TAG_NIL_EXT);
        Ok(())
    }
    else if length <= 65535 {
        data.push(TAG_STRING_EXT);
        pack_u16(length as u16, data);
        data.extend(value);
        Ok(())
    }
    else if length <= 4294967295 {
        data.push(TAG_LIST_EXT);
        pack_u32(length as u32, data);
        for c in value.iter() {
            data.push(TAG_SMALL_INTEGER_EXT);
            data.push(*c);
        }
        data.push(TAG_NIL_EXT);
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u32 overflow").into())
    }
}

fn tuple_to_binary(value: &Vec<OtpErlangTerm>,
                   data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if length <= 255 {
        data.push(TAG_SMALL_TUPLE_EXT);
        data.push(length as u8);
        for term in value.iter() {
            let _ = term_to_binary_(term, data)?;
        }
        Ok(())
    }
    else if length <= 4294967295 {
        data.push(TAG_LARGE_TUPLE_EXT);
        pack_u32(length as u32, data);
        for term in value.iter() {
            let _ = term_to_binary_(term, data)?;
        }
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u32 overflow").into())
    }
}

fn list_to_binary(value: &Vec<OtpErlangTerm>, improper: bool,
                  data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if length == 0 {
        data.push(TAG_NIL_EXT);
        Ok(())
    }
    else if length <= 4294967295 {
        data.push(TAG_LIST_EXT);
        if improper {
            pack_u32((length - 1) as u32, data);
        }
        else {
            pack_u32(length as u32, data);
        }
        for term in value.iter() {
            let _ = term_to_binary_(term, data)?;
        }
        if ! improper {
            data.push(TAG_NIL_EXT);
        }
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u32 overflow").into())
    }
}

fn integer_to_binary(value: i32, data: &mut Vec<u8>) -> Result<()> {
    if value >= 0 && value <= 255 {
        data.push(TAG_SMALL_INTEGER_EXT);
        data.push(value as u8);
        Ok(())
    }
    else {
        data.push(TAG_INTEGER_EXT);
        pack_u32(value as u32, data);
        Ok(())
    }
}

fn atom_to_binary(value: &Vec<u8>, data: &mut Vec<u8>) -> Result<()> {
    // deprecated
    // (not used in Erlang/OTP 26, i.e., minor_version 2)
    let length = value.len();
    if length <= 255 {
        data.push(TAG_SMALL_ATOM_EXT);
        data.push(length as u8);
        data.extend(value);
        Ok(())
    }
    else if length <= 65535 {
        data.push(TAG_ATOM_EXT);
        pack_u16(length as u16, data);
        data.extend(value);
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u16 overflow").into())
    }
}

fn atom_utf8_to_binary(value: &Vec<u8>, data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if length <= 255 {
        data.push(TAG_SMALL_ATOM_UTF8_EXT);
        data.push(length as u8);
        data.extend(value);
        Ok(())
    }
    else if length <= 65535 {
        data.push(TAG_ATOM_UTF8_EXT);
        pack_u16(length as u16, data);
        data.extend(value);
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u16 overflow").into())
    }
}

fn binary_bits_to_binary(value: &Vec<u8>, bits: u8,
                         data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if bits == 0 {
        Err(ErrorKind::OutputError("invalid OtpErlangBinaryBits").into())
    }
    else if length <= 4294967295 {
        if bits != 8 {
            data.push(TAG_BIT_BINARY_EXT);
            pack_u32(length as u32, data);
            data.push(bits);
        }
        else {
            data.push(TAG_BINARY_EXT);
            pack_u32(length as u32, data);
        }
        data.extend(value);
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u32 overflow").into())
    }
}

fn map_to_binary(value: &BTreeMap<OtpErlangTerm, OtpErlangTerm>,
                 data: &mut Vec<u8>) -> Result<()> {
    let length = value.len();
    if length <= 4294967295 {
        data.push(TAG_MAP_EXT);
        pack_u32(length as u32, data);
        for (k, v) in value {
            let () = term_to_binary_(k, data)?;
            let () = term_to_binary_(v, data)?;
        }
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u32 overflow").into())
    }
}

fn pid_to_binary(value: &Pid, data: &mut Vec<u8>) -> Result<()> {
    if value.creation.len() == 4 {
        data.push(TAG_NEW_PID_EXT);
    }
    else {
        data.push(TAG_PID_EXT);
    }
    data.push(value.node_tag);
    data.extend(&value.node);
    data.extend(&value.id);
    data.extend(&value.serial);
    data.extend(&value.creation);
    Ok(())
}

fn port_to_binary(value: &Port, data: &mut Vec<u8>) -> Result<()> {
    if value.id.len() == 8 {
        data.push(TAG_V4_PORT_EXT);
    }
    else if value.creation.len() == 4 {
        data.push(TAG_NEW_PORT_EXT);
    }
    else {
        data.push(TAG_PORT_EXT);
    }
    data.push(value.node_tag);
    data.extend(&value.node);
    data.extend(&value.id);
    data.extend(&value.creation);
    Ok(())
}

fn reference_to_binary(value: &Reference, data: &mut Vec<u8>) -> Result<()> {
    let length = value.id.len() / 4;
    if length == 0 {
        data.push(TAG_REFERENCE_EXT);
        data.push(value.node_tag);
        data.extend(&value.node);
        data.extend(&value.id);
        data.extend(&value.creation);
        Ok(())
    }
    else if length <= 65535 {
        if value.creation.len() == 4 {
            data.push(TAG_NEWER_REFERENCE_EXT);
        }
        else {
            data.push(TAG_NEW_REFERENCE_EXT);
        }
        pack_u16(length as u16, data);
        data.push(value.node_tag);
        data.extend(&value.node);
        data.extend(&value.creation);
        data.extend(&value.id);
        Ok(())
    }
    else {
        Err(ErrorKind::OutputError("u16 overflow").into())
    }
}

fn function_to_binary(value: &Function, data: &mut Vec<u8>) -> Result<()> {
    data.push(value.tag);
    data.extend(&value.value);
    Ok(())
}

// MIT LICENSE (of tests below)
//
// Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>
// Copyright (c) 2009-2013 Dmitry Vasiliev <dima@hlabs.org>
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

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_pid() {
        let pid1 = OtpErlangTerm::OtpErlangPid(Pid {
            node_tag: 100,
            node: vec![
                b"\x00\x0d\x6e\x6f\x6e\x6f\x64\x65".to_vec(),
                b"\x40\x6e\x6f\x68\x6f\x73\x74".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>(),
            id: b"\x00\x00\x00\x3b".to_vec(),
            serial: b"\x00\x00\x00\x00".to_vec(),
            creation: b"\x00".to_vec(),
        });
        let binary1 = vec![
            b"\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65".to_vec(),
            b"\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x3B".to_vec(),
            b"\x00\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let pid2 = OtpErlangTerm::OtpErlangPid(Pid {
            node_tag: 119,
            node: vec![
                b"\x0D\x6E\x6F\x6E\x6F\x64\x65\x40".to_vec(),
                b"\x6E\x6F\x68\x6F\x73\x74".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>(),
            id: b"\x00\x00\x00\x50".to_vec(),
            serial: b"\x00\x00\x00\x00".to_vec(),
            creation: b"\x00".to_vec(),
        });
        let binary2 = vec![
            b"\x83\x67\x77\x0D\x6E\x6F\x6E\x6F\x64\x65\x40".to_vec(),
            b"\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x50\x00".to_vec(),
            b"\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        assert_eq!(binary_to_term(binary1.as_slice()).unwrap(), pid1);
        assert_eq!(term_to_binary(&pid1).unwrap(), binary1.as_slice());
        assert_eq!(binary_to_term(binary2.as_slice()).unwrap(), pid2);
        assert_eq!(term_to_binary(&pid2).unwrap(), binary2.as_slice());
        let pid_old_binary = vec![
            b"\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65".to_vec(),
            b"\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x4E".to_vec(),
            b"\x00\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let pid_old = binary_to_term(pid_old_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&pid_old).unwrap(),
                   pid_old_binary.as_slice());
        let pid_new_binary = vec![
            b"\x83\x58\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65".to_vec(),
            b"\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x4E".to_vec(),
            b"\x00\x00\x00\x00\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let pid_new = binary_to_term(pid_new_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&pid_new).unwrap(),
                   pid_new_binary.as_slice());
    }

    #[test]
    fn test_port() {
        let port_old_binary = vec![
            b"\x83\x66\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65".to_vec(),
            b"\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x06\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let port_old = binary_to_term(port_old_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&port_old).unwrap(),
                   port_old_binary.as_slice());
        let port_new_binary = vec![
            b"\x83\x59\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65".to_vec(),
            b"\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x06".to_vec(),
            b"\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let port_new = binary_to_term(port_new_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&port_new).unwrap(),
                   port_new_binary.as_slice());
        let port_v4_binary = vec![
            b"\x83\x78\x77\x0D\x6E\x6F\x6E\x6F\x64\x65\x40".to_vec(),
            b"\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\x00".to_vec(),
            b"\x00\x00\x04\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let port_v4 = binary_to_term(port_v4_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&port_v4).unwrap(),
                   port_v4_binary.as_slice());
    }

    #[test]
    fn test_reference() {
        let reference1 = OtpErlangTerm::OtpErlangReference(Reference {
            node_tag: 100,
            node: vec![
                b"\x00\x0d\x6e\x6f\x6e\x6f\x64\x65\x40\x6e".to_vec(),
                b"\x6f\x68\x6f\x73\x74".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>(),
            id: b"\x00\x00\x00\xaf\x00\x00\x00\x03\x00\x00\x00\x00".to_vec(),
            creation: b"\x00".to_vec(),
        });
        let binary1 = vec![
            b"\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F".to_vec(),
            b"\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00".to_vec(),
            b"\x00\x00\xAF\x00\x00\x00\x03\x00\x00\x00\x00".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        assert_eq!(binary_to_term(binary1.as_slice()).unwrap(), reference1);
        assert_eq!(term_to_binary(&reference1).unwrap(), binary1.as_slice());
        let ref_new_binary = vec![
            b"\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F".to_vec(),
            b"\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00".to_vec(),
            b"\x03\xE8\x4E\xE7\x68\x00\x02\xA4\xC8\x53\x40".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let ref_new = binary_to_term(ref_new_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&ref_new).unwrap(),
                   ref_new_binary.as_slice());
        let ref_newer_binary = vec![
            b"\x83\x5A\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F".to_vec(),
            b"\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00".to_vec(),
            b"\x00\x00\x00\x01\xAC\x03\xC7\x00\x00\x04\xBB".to_vec(),
            b"\xB2\xCA\xEE".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        let ref_newer = binary_to_term(ref_newer_binary.as_slice()).unwrap();
        assert_eq!(term_to_binary(&ref_newer).unwrap(),
                   ref_newer_binary.as_slice());
    }

    #[test]
    fn test_function() {
        let function1 = OtpErlangTerm::OtpErlangFunction(Function {
            tag: 113,
            value: vec![
                b"\x64\x00\x05\x6c\x69\x73\x74\x73\x64\x00".to_vec(),
                b"\x06\x6d\x65\x6d\x62\x65\x72\x61\x02".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>(),
        });
        let binary1 = vec![
            b"\x83\x71\x64\x00\x05\x6C\x69\x73\x74\x73\x64".to_vec(),
            b"\x00\x06\x6D\x65\x6D\x62\x65\x72\x61\x02".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        assert_eq!(binary_to_term(binary1.as_slice()).unwrap(), function1);
        assert_eq!(term_to_binary(&function1).unwrap(), binary1.as_slice());
    }

    #[test]
    fn test_decode_basic() {
        assert_eq!(binary_to_term(b"").unwrap_err(),
                   ErrorKind::ParseError("null input").into());
        assert_eq!(binary_to_term(b"\x83").unwrap_err(),
                   ErrorKind::ParseError("null input").into());
        assert_eq!(binary_to_term(b"\x83z").unwrap_err(),
                   ErrorKind::ParseError("invalid tag").into());
    }

    #[test]
    fn test_decode_atom() {
        assert_eq!(binary_to_term(b"\x83d").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83s\x00").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83d\x00\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"test".to_vec()));
        assert_eq!(binary_to_term(b"\x83s\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"test".to_vec()));
    }

    #[test]
    fn test_decode_predefined_atom() {
        assert_eq!(binary_to_term(b"\x83s\x04true").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(true));
        assert_eq!(binary_to_term(b"\x83s\x05false").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(false));
        assert_eq!(binary_to_term(b"\x83s\x09undefined").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"undefined".to_vec()));
        assert_eq!(binary_to_term(b"\x83w\x04true").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(true));
        assert_eq!(binary_to_term(b"\x83w\x05false").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(false));
        assert_eq!(binary_to_term(b"\x83w\x09undefined").unwrap(),
                   OtpErlangTerm::OtpErlangAtomUTF8(b"undefined".to_vec()));
    }

    #[test]
    fn test_decode_empty_list() {
        assert_eq!(binary_to_term(b"\x83j").unwrap(),
                   OtpErlangTerm::OtpErlangList(Vec::new()));
    }

    #[test]
    fn test_decode_string_list() {
        assert_eq!(binary_to_term(b"\x83k").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangString(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83k\x00\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangString(b"test".to_vec()));
    }

    #[test]
    fn test_decode_list() {
        assert_eq!(binary_to_term(b"\x83l").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00j").unwrap(),
                   OtpErlangTerm::OtpErlangList(Vec::new()));
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x02jjj").unwrap(),
                   OtpErlangTerm::OtpErlangList(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_improper_list() {
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00k").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83l\x00\x00\x00\x01jd\x00\x04tail").unwrap(),
            OtpErlangTerm::OtpErlangListImproper(vec![
                OtpErlangTerm::OtpErlangList(Vec::new()),
                OtpErlangTerm::OtpErlangAtom(b"tail".to_vec()),
            ]));
    }

    #[test]
    fn test_decode_small_tuple() {
        assert_eq!(binary_to_term(b"\x83h").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83h\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83h\x00").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(Vec::new()));
        assert_eq!(binary_to_term(b"\x83h\x02jj").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_large_tuple() {
        assert_eq!(binary_to_term(b"\x83i").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(Vec::new()));
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x02jj").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_small_integer() {
        assert_eq!(binary_to_term(b"\x83a").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83a\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(0));
        assert_eq!(binary_to_term(b"\x83a\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(255));
    }

    #[test]
    fn test_decode_integer() {
        assert_eq!(binary_to_term(b"\x83b").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(0));
        assert_eq!(binary_to_term(b"\x83b\x7f\xff\xff\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(i32::MAX));
        assert_eq!(binary_to_term(b"\x83b\x80\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(i32::MIN));
        assert_eq!(binary_to_term(b"\x83b\xff\xff\xff\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(-1));
    }

    #[test]
    fn test_decode_binary() {
        assert_eq!(binary_to_term(b"\x83m").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangBinary(Vec::new()));
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x04data").unwrap(),
                   OtpErlangTerm::OtpErlangBinary(b"data".to_vec()));
    }

    #[test]
    fn test_decode_float() {
        assert_eq!(binary_to_term(b"\x83F").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00").unwrap_err(),
            ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00\x00").unwrap_err(),
            ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00\x00\x00").unwrap(),
            OtpErlangTerm::OtpErlangFloat(0.0.into()));
        assert_eq!(
            binary_to_term(b"\x83F?\xf8\x00\x00\x00\x00\x00\x00").unwrap(),
            OtpErlangTerm::OtpErlangFloat(1.5.into()));
    }

    #[test]
    fn test_decode_map() {
        assert_eq!(binary_to_term(b"\x83t").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83t\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83t\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83t\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83t\x00\x00\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83t\x00\x00\x00\x00").unwrap(),
            OtpErlangTerm::OtpErlangMap(BTreeMap::new()));
        let mut map1 = BTreeMap::new();
        map1.insert(OtpErlangTerm::OtpErlangAtom(b"a".to_vec()),
                    OtpErlangTerm::OtpErlangInteger(1));
        assert_eq!(
            binary_to_term(b"\x83t\x00\x00\x00\x01d\x00\x01aa\x01").unwrap(),
            OtpErlangTerm::OtpErlangMap(map1));
        let mut map2 = BTreeMap::new();
        map2.insert(OtpErlangTerm::OtpErlangBinaryBits(b"\xA8".to_vec(), 6),
                    OtpErlangTerm::OtpErlangBinary(b"everything".to_vec()));
        map2.insert(OtpErlangTerm::OtpErlangAtomUTF8(b"undefined".to_vec()),
                    OtpErlangTerm::OtpErlangBinary(b"nothing".to_vec()));
        let binary2 = vec![
            b"\x83\x74\x00\x00\x00\x02\x77\x09\x75\x6E\x64\x65".to_vec(),
            b"\x66\x69\x6E\x65\x64\x6D\x00\x00\x00\x07\x6E\x6F".to_vec(),
            b"\x74\x68\x69\x6E\x67\x4D\x00\x00\x00\x01\x06\xA8".to_vec(),
            b"\x6D\x00\x00\x00\x0A\x65\x76\x65\x72\x79\x74\x68".to_vec(),
            b"\x69\x6E\x67".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        assert_eq!(
            binary_to_term(binary2.as_slice()).unwrap(),
            OtpErlangTerm::OtpErlangMap(map2));
    }

    #[test]
    fn test_encode_tuple() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangTuple(vec![])).unwrap(),
            b"\x83h\x00");
        let tuple1 = OtpErlangTerm::OtpErlangTuple(vec![
            OtpErlangTerm::OtpErlangTuple(Vec::new()),
            OtpErlangTerm::OtpErlangTuple(vec![]),
        ]);
        assert_eq!(term_to_binary(&tuple1).unwrap(),
                   b"\x83h\x02h\x00h\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangTuple(vec![
                OtpErlangTerm::OtpErlangTuple(Vec::new()); 255])).unwrap(),
            [b"\x83h\xff",
             b"h\x00".repeat(255).as_slice()].concat());
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangTuple(vec![
                OtpErlangTerm::OtpErlangTuple(Vec::new()); 256])).unwrap(),
            [b"\x83i\x00\x00\x01\x00",
             b"h\x00".repeat(256).as_slice()].concat());
    }


    #[test]
    fn test_encode_empty_list() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![])).unwrap(),
            b"\x83j");
    }

    #[test]
    fn test_encode_string_list() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"".to_vec())).unwrap(),
            b"\x83j");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\x00".to_vec())).unwrap(),
            b"\x83k\x00\x01\x00");
        // concat_bytes! isn't available yet
        let s = vec![
            b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r".to_vec(),
            b"\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a".to_vec(),
            b"\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>".to_vec(),
            b"?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopq".to_vec(),
            b"rstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88".to_vec(),
            b"\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95".to_vec(),
            b"\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2".to_vec(),
            b"\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf".to_vec(),
            b"\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc".to_vec(),
            b"\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9".to_vec(),
            b"\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6".to_vec(),
            b"\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3".to_vec(),
            b"\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0".to_vec(),
            b"\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd".to_vec(),
            b"\xfe\xff".to_vec(),
        ].into_iter().flatten().collect::<Vec<u8>>();
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                s.clone())).unwrap(),
            [b"\x83k\x01\x00", s.as_slice()].concat());
    }

    #[test]
    fn test_encode_list_basic() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                Vec::new())).unwrap(),
            b"\x83\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangString(b"".to_vec())])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x6A\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(1)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x61\x01\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(255)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(256)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(i32::MAX)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(0)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x61\x00\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(-1)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(-256)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(-257)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(i32::MIN)])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangString(b"test".to_vec())])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangInteger(373),
                OtpErlangTerm::OtpErlangInteger(455)])).unwrap(),
            [b"\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01",
             b"\x75\x62\x00\x00\x01\xC7\x6A".to_vec().as_slice()].concat());
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangList(vec![])])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x01\x6A\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangList(vec![]),
                OtpErlangTerm::OtpErlangList(vec![])])).unwrap(),
            b"\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangList(vec![
                    OtpErlangTerm::OtpErlangString(b"this".to_vec()),
                    OtpErlangTerm::OtpErlangString(b"is".to_vec())]),
                OtpErlangTerm::OtpErlangList(vec![
                    OtpErlangTerm::OtpErlangList(vec![
                        OtpErlangTerm::OtpErlangString(b"a".to_vec())])]),
                OtpErlangTerm::OtpErlangString(b"test".to_vec()),
            ])).unwrap(),
            vec![
                b"\x83\x6C\x00\x00\x00\x03\x6C\x00".to_vec(),
                b"\x00\x00\x02\x6B\x00\x04\x74\x68".to_vec(),
                b"\x69\x73\x6B\x00\x02\x69\x73\x6A".to_vec(),
                b"\x6C\x00\x00\x00\x01\x6C\x00\x00".to_vec(),
                b"\x00\x01\x6B\x00\x01\x61\x6A\x6A".to_vec(),
                b"\x6B\x00\x04\x74\x65\x73\x74\x6A".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>().as_slice());
    }

    #[test]
    fn test_encode_list() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangList(vec![])])).unwrap(),
            b"\x83l\x00\x00\x00\x01jj");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangList(vec![
                OtpErlangTerm::OtpErlangList(vec![]),
                OtpErlangTerm::OtpErlangList(vec![]),
                OtpErlangTerm::OtpErlangList(vec![]),
                OtpErlangTerm::OtpErlangList(vec![]),
                OtpErlangTerm::OtpErlangList(vec![])])).unwrap(),
            b"\x83l\x00\x00\x00\x05jjjjjj");
    }

    #[test]
    fn test_encode_improper_list() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangListImproper(vec![
                OtpErlangTerm::OtpErlangTuple(vec![]),
                OtpErlangTerm::OtpErlangTuple(vec![])])).unwrap(),
            b"\x83l\x00\x00\x00\x01h\x00h\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangListImproper(vec![
                OtpErlangTerm::OtpErlangInteger(0),
                OtpErlangTerm::OtpErlangInteger(1)])).unwrap(),
            b"\x83l\x00\x00\x00\x01a\x00a\x01");
    }

    #[test]
    fn test_encode_unicode() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"".to_vec())).unwrap(),
            b"\x83j");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"test".to_vec())).unwrap(),
            b"\x83k\x00\x04test");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\x00\xc3\xbf".to_vec())).unwrap(),
            b"\x83k\x00\x03\x00\xc3\xbf");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\xc4\x80".to_vec())).unwrap(),
            b"\x83k\x00\x02\xc4\x80");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\xd1\x82\xd0\xb5\xd1\x81\xd1\x82".to_vec())).unwrap(),
            b"\x83k\x00\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\xd0\x90".repeat(65536).as_slice().to_vec())).unwrap(),
            vec![
                b"\x83l\x00\x02\x00\x00".to_vec(),
                b"a\xd0a\x90".repeat(65536).as_slice().to_vec(),
                b"j".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>().as_slice());
    }

    #[test]
    fn test_encode_atom() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtom(
                b"".to_vec())).unwrap(),
            b"\x83s\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtom(
                b"test".to_vec())).unwrap(),
            b"\x83s\x04test");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtomUTF8(
                b"".to_vec())).unwrap(),
            b"\x83w\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtomUTF8(
                b"test".to_vec())).unwrap(),
            b"\x83w\x04test");
    }

    #[test]
    fn test_encode_string_basic() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"".to_vec())).unwrap(),
            b"\x83\x6A");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"test".to_vec())).unwrap(),
            b"\x83\x6B\x00\x04\x74\x65\x73\x74");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"two words".to_vec())).unwrap(),
            b"\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"testing multiple words".to_vec())).unwrap(),
            vec![
                b"\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E".to_vec(),
                b"\x67\x20\x6D\x75\x6C\x74\x69\x70\x6C\x65".to_vec(),
                b"\x20\x77\x6F\x72\x64\x73".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>().as_slice());
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b" ".to_vec())).unwrap(),
            b"\x83\x6B\x00\x01\x20");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"  ".to_vec())).unwrap(),
            b"\x83\x6B\x00\x02\x20\x20");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"1".to_vec())).unwrap(),
            b"\x83\x6B\x00\x01\x31");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"37".to_vec())).unwrap(),
            b"\x83\x6B\x00\x02\x33\x37");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"one = 1".to_vec())).unwrap(),
            b"\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"!@#$%^&*()_+-=[]{}\\|;':\",./<>?~`".to_vec())).unwrap(),
            vec![
                b"\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A".to_vec(),
                b"\x28\x29\x5F\x2B\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C".to_vec(),
                b"\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C\x3E\x3F\x7E\x60".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>().as_slice());
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"\"\x08\x0c\n\r\t\x0bS\x12".to_vec())).unwrap(),
            b"\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12");
    }

    #[test]
    fn test_encode_string() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"".to_vec())).unwrap(),
            b"\x83j");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangString(
                b"test".to_vec())).unwrap(),
            b"\x83k\x00\x04test");
    }

    #[test]
    fn test_encode_boolean() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtomBool(true)).unwrap(),
            b"\x83w\x04true");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangAtomBool(false)).unwrap(),
            b"\x83w\x05false");
    }

    #[test]
    fn test_encode_small_integer() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(0)).unwrap(),
            b"\x83a\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(255)).unwrap(),
            b"\x83a\xff");
    }

    #[test]
    fn test_encode_integer() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(-1)).unwrap(),
            b"\x83b\xff\xff\xff\xff");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(i32::MIN)).unwrap(),
            b"\x83b\x80\x00\x00\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(256)).unwrap(),
            b"\x83b\x00\x00\x01\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangInteger(i32::MAX)).unwrap(),
            b"\x83b\x7f\xff\xff\xff");
    }

    #[test]
    fn test_encode_float() {
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangFloat(
                0.0.into())).unwrap(),
            b"\x83F\x00\x00\x00\x00\x00\x00\x00\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangFloat(
                0.5.into())).unwrap(),
            b"\x83F?\xe0\x00\x00\x00\x00\x00\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangFloat(
                (-0.5).into())).unwrap(),
            b"\x83F\xbf\xe0\x00\x00\x00\x00\x00\x00");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangFloat(
                3.1415926.into())).unwrap(),
            b"\x83F@\t!\xfbM\x12\xd8J");
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangFloat(
                (-3.1415926).into())).unwrap(),
            b"\x83F\xc0\t!\xfbM\x12\xd8J");
    }

    #[test]
    fn test_encode_map() {
        assert_eq!(
            term_to_binary(
                &OtpErlangTerm::OtpErlangMap(BTreeMap::new())).unwrap(),
            b"\x83t\x00\x00\x00\x00");
        let mut map1 = BTreeMap::new();
        map1.insert(OtpErlangTerm::OtpErlangAtom(b"a".to_vec()),
                    OtpErlangTerm::OtpErlangInteger(1));
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangMap(map1)).unwrap(),
            b"\x83t\x00\x00\x00\x01s\x01aa\x01");
        let mut map2 = BTreeMap::new();
        map2.insert(OtpErlangTerm::OtpErlangBinaryBits(b"\xA8".to_vec(), 6),
                    OtpErlangTerm::OtpErlangBinary(b"everything".to_vec()));
        map2.insert(OtpErlangTerm::OtpErlangAtomUTF8(b"undefined".to_vec()),
                    OtpErlangTerm::OtpErlangBinary(b"nothing".to_vec()));
        assert_eq!(
            term_to_binary(&OtpErlangTerm::OtpErlangMap(map2)).unwrap(),
            vec![
                b"\x83\x74\x00\x00\x00\x02\x77\x09".to_vec(),
                b"\x75\x6E\x64\x65\x66\x69\x6E\x65".to_vec(),
                b"\x64\x6D\x00\x00\x00\x07\x6E\x6F".to_vec(),
                b"\x74\x68\x69\x6E\x67\x4D\x00\x00".to_vec(),
                b"\x00\x01\x06\xA8\x6D\x00\x00\x00".to_vec(),
                b"\x0A\x65\x76\x65\x72\x79\x74\x68".to_vec(),
                b"\x69\x6E\x67".to_vec(),
            ].into_iter().flatten().collect::<Vec<u8>>().as_slice());
    }
}

