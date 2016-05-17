%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @author: shino@accense.com

Definitions.

%% Characters for keys
K = [a-zA-Z0-9_\.]+

%% Characters for values, printable except =, [ and ]
%% \x3b : $;
%% \x3d : $=
%% \x5b : $[
%% \x5d : $]
V = [\x21-\x3a\x3c\x3e-\x5a\x5c\x5e-\x7e]+

%% spaces and breaks
S = [\s\t]
B = [\n\r]

Rules.

%% skip empty or blank lines or lines with space/tab chars
{B}({S}*{B})+    : {token, {break,   TokenLine}}.

%% mark line break by token 'break' in order to use as delimiters
{B}              : {token, {break,   TokenLine}}.

%% Just chars
=                : {token, {'=',     TokenLine}}.
\[               : {token, {'[',     TokenLine}}.
\]               : {token, {']',     TokenLine}}.

%% word-like tokens
{S}+             : {token, {blank,   TokenLine, TokenChars}}.
{K}              : {token, {word,    TokenLine, TokenChars}}.
{V}              : {token, {value,   TokenLine, TokenChars}}.

%% comment-like token, but may be a part of value depending on the location
;.*              : {token, {comment, TokenLine, TokenChars}}.

Erlang code.
