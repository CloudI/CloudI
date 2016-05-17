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

-module(eini).

-author('shino@accense.com').
-author('nakai@accense.com').

-export([parse/1]).

-export([lookup_value/3,
         register/2,
         register/4,
         is_section/2]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% for debug use
-export([lex/1, parse_tokens/1]).

-define(EINI_TABLE, eini_table).

-type sections() :: [section()].
-type section() :: {Title::atom(), [property()]}.
-type property() :: {Key::atom(), Value::binary()}.

-type reason() :: {illegal_character, Line::integer(), Reason::string()}
                | {syntax_error, Line::integer(), Reason::string()}
                | {duplicate_title, Title::binary()}
                | {duplicate_key, Title::binary(), Key::binary()}.

-spec parse(Content:: string() | binary()) -> {ok, sections()}
                                            | {error, reason()}.
parse(Content) when is_binary(Content) ->
  parse(binary_to_list(Content));
parse(Content) when is_list(Content) ->
  case lex(Content) of
    {ok, Tokens} ->
      parse_and_validate(Tokens);
    {error, Reason} ->
      {error, Reason}
  end.

parse_and_validate(Tokens) ->
  case parse_tokens(Tokens) of
    {ok, Parsed} ->
      validate(Parsed);
    {error, Reason} ->
      {error, Reason}
  end.

-spec lex(string()) -> {ok, list(Token::tuple())}
                     | {error, {illegal_character, Line::integer(), Reason::string()}}.
lex(String) when is_list(String) ->
  %% Add \n char at the end if does NOT end by \n
  %% TOD(shino): more simple logic?
  String2 = case String of
              "" ->
                "\n";
              _NotEmpty ->
                case lists:last(String) of
                  $\n ->
                    String;
                  _ ->
                    String ++ "\n"
                end
            end,
  case eini_lexer:string(String2) of
    {ok, [{break, _Line}|RestTokens], _EndLine} ->
      {ok, RestTokens};
    {ok, Tokens, _EndLine} ->
      {ok, Tokens};
    {error, {ErrorLine, Mod, Reason}, _EndLine} ->
      {error, {illegal_character, ErrorLine, Mod:format_error(Reason)}}
  end.
  
-spec parse_tokens(Token::tuple()) ->
                      {ok, sections()}
                    | {error, {syntax_error, Line::integer(), Reason::string()}}.
parse_tokens(Tokens) ->
  case eini_parser:parse(Tokens) of
    {ok, Res} ->
      {ok, Res};
    {error, {Line, Mod, Reason}} ->
      {error, {syntax_error, Line, Mod:format_error(Reason)}}
  end.

-spec validate(sections()) ->
                      {ok, sections()}
                    | {error, {duplicate_title, Title::binary()}}
                    | {error, {duplicate_key, Title::binary(), Key::binary()}}.
validate(Sections) ->
  validate(Sections, [], []).

validate([], _AccTitles, AccSections) ->
  {ok, lists:reverse(AccSections)};
validate([{Title, Properties} = Section | Sections], AccTitles, AccSections) ->
  case lists:member(Title, AccTitles) of
    true ->
      {error, {duplicate_title, Title}};
    false ->
      validate(Sections, [Title|AccTitles], [Section|AccSections], Properties, [])
  end.

validate(Sections, AccTitles, AccSections, [], _AccKeys) ->
  validate(Sections, AccTitles, AccSections);
validate(Sections, AccTitles, AccSections, [{Key, _Value}|Properties], AccKeys) ->
  case lists:member(Key, AccKeys) of
    true ->
      {error, {duplicate_key, hd(AccTitles), Key}};
    false ->
      validate(Sections, AccTitles, AccSections, Properties, [Key|AccKeys])
  end.

-spec lookup_value(file:name(), atom(), atom()) -> not_found | binary().
lookup_value(Filename, Section, Key) ->
  case ets:lookup(?EINI_TABLE, {Filename, Section, Key}) of
    [] ->
      not_found;
    [{_, Value}] ->
      Value
  end.

-spec register(file:name(), binary()) -> ok | {error, reason()}.
register(Filename, Binary) ->
  gen_server:call(?MODULE, {register, Filename, Binary}).  

-spec register(file:name(), atom(), atom(), any()) -> ok | {error, duplicate_key}.
register(Filename, Section, Key, Value) when is_atom(Section) andalso is_atom(Key) ->
  gen_server:call(?MODULE, {register, Filename, Section, Key, Value}).  

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  process_flag(trap_exit, true),
  Options = [set, protected, named_table, {read_concurrency, true}],
  _Tid = ets:new(?EINI_TABLE, Options),
  {ok, {}}.

handle_call({register, Filename, Section, Key, Value}, _From, State) ->
  case ets:insert_new(?EINI_TABLE, {{Filename, Section, Key}, Value}) of
    true ->
      {reply, ok, State};
    false ->
      {reply, {error, {duplicate_key, Section, Key}}, State}
  end;
handle_call({register, Filename, Binary}, _From, State) ->
  case eini:parse(Binary) of
    {ok, Sections} ->
      case insert_sections(Filename, Sections) of
        ok ->
          true = ets:insert_new(?EINI_TABLE, {Filename, Binary}),
          {reply, ok, State};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec is_section(file:name(), atom()) -> boolean().
is_section(Filename, Section) ->
  case ets:match_object(?EINI_TABLE, {{Filename, Section, '_'}, '_'}) of
    [] ->
      false;
    _ ->
      true
  end.

-spec insert_sections(file:name(), [{atom(), [property()]}]) -> ok.
insert_sections(_Filename, []) ->
  ok;
insert_sections(Filename, [{Section, ListOfProperty}|ListOfSection]) ->
  insert_section(Filename, ListOfSection, Section, ListOfProperty).

-spec insert_section(file:name(), sections(), atom(), [property()]) -> ok.
insert_section(Filename, ListOfSection, _Section, []) ->
  insert_sections(Filename, ListOfSection);
insert_section(Filename, ListOfSection, Section, [{Key, Value}|ListOfProperty]) ->
  case ets:insert_new(?EINI_TABLE, {{Filename, Section, Key}, Value}) of
    true ->
      insert_section(Filename, ListOfSection, Section, ListOfProperty);
    false ->
      {error, {duplicate_key, Section, Key}}
  end.
