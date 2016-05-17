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

Nonterminals
  whole
  sections
  sections_with_skip_lines
  section
  title_part
  title
  property_with_skip_lines
  properties property
  key_part
  values single_value
  skip_lines
  comment_line
  blank_line.

Terminals
  '[' ']' '='
  blank
  word
  value
  comment
  break.

Rootsymbol whole.

%% Leex smash out all blank lines, but at the beggining of file is NOT.
whole -> sections_with_skip_lines : '$1'.
whole -> blank_line sections_with_skip_lines : '$2'.

blank_line -> blank break : '$1'.

%% Comment lines are treated as:
%% 1. At the biggining of file are included into sections_with_skip_lines
%% 2. Other comment lines are included to title_part and property_with_skip_lines

skip_lines -> comment_line : ['$1'].
skip_lines -> comment_line skip_lines : ['$1', '$2'].

comment_line -> comment break : '$1'.

sections_with_skip_lines -> sections : '$1'.
sections_with_skip_lines -> skip_lines sections : '$2'.

sections -> '$empty' : [].
sections -> section sections : ['$1' | '$2'].

section -> title_part properties : {'$1', '$2'}.

title_part -> title break                  : list_to_atom('$1').
title_part -> title blank break            : list_to_atom('$1').
title_part -> title break skip_lines       : list_to_atom('$1').
title_part -> title blank break skip_lines : list_to_atom('$1').

title -> '[' word ']'              : value_of('$2').

properties -> '$empty' : [].
properties -> property_with_skip_lines properties : ['$1' | '$2'].

property_with_skip_lines -> property : '$1'.
property_with_skip_lines -> property skip_lines : '$1'.

property -> key_part '=' values break :
              {list_to_atom(value_of('$1')), strip_values('$3')}.

key_part -> word : '$1'.
key_part -> word blank : '$1'.
key_part -> blank word : '$2'.
key_part -> blank word blank : '$2'.

values -> single_value : ['$1'].
values -> single_value values : ['$1' | '$2'].

%% At value position, any characters are accepted AS IS.
single_value ->  word    : value_of('$1'). 
single_value ->  value   : value_of('$1').
single_value ->  blank   : value_of('$1').
single_value ->  comment : value_of('$1').
single_value -> '['      : "[".
single_value -> '='      : "=".
single_value -> ']'      : "]".

Erlang code.

-spec value_of({atom(), _Line, TokenChars::string()}) -> TokenChars::string().
value_of(Token) ->
  element(3, Token).

-spec strip_values([TokenChars::string()]) -> Value::binary().
strip_values(Values) ->
  String = string:strip(string:strip(lists:flatten(Values), both, $\s), both, $\t),
  list_to_binary(String).
