#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_table").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(trim(Str), string:trim(Str, both)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-else.
-define(trim(Str), string:strip(Str, both)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-endif.


main(_) ->
  {ok, IM} = file:open("../uc_spec/idna-table.txt", [read, raw, {read_ahead, 1000000}]),
  Data = foldl(fun parse_idna_table/2, [], IM),
  file:close(IM),

  %% Make module
  OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
  {ok, Out} = file:open(OutputPath, [write]),
  gen_file(Out, Data),
  ok = file:close(Out),
  ok.


parse_idna_table(Line0, Acc) ->
  [Line|_Comments] = tokens(Line0, "#"),
  [CodePoints, Status] = tokens(Line, ";"),
  [{to_range(CodePoints), to_atom(Status)} | Acc].


gen_file(Fd, Data) ->
  gen_header(Fd),
  gen_disallowed_p(Fd),
  gen_contextj_p(Fd),
  gen_contexto_p(Fd),
  gen_unassigned_p(Fd),
  gen_valid_p(Fd),
  gen_lookup(Fd, Data),
  ok.


gen_header(Fd) ->
  io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
  io:put_chars(Fd, "%% see ../uc_spec/gen_idna_table.escript\n\n"),
  io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
  io:put_chars(Fd, "-compile(compressed).\n"),
  io:put_chars(Fd, "-export([lookup/1]).\n"),
  io:put_chars(Fd, "-export([disallowed_p/1, contextj_p/1, contexto_p/1, unassigned_p/1, valid_p/1]).\n"),
  ok.

gen_disallowed_p(Fd) ->
  io:put_chars(Fd, "disallowed_p(CP) -> lookup(CP) == 'DISALLOWED'.\n").

gen_contextj_p(Fd) ->
  io:put_chars(Fd, "contextj_p(CP) -> lookup(CP) == 'CONTEXTJ'.\n").

gen_contexto_p(Fd) ->
  io:put_chars(Fd, "contexto_p(CP) -> lookup(CP) == 'CONTEXTO'.\n").

gen_unassigned_p(Fd) ->
  io:put_chars(Fd, "unassigned_p(CP) -> lookup(CP) == 'UNASSIGNED'.\n").

gen_valid_p(Fd) ->
  io:put_chars(Fd, "valid_p(CP) -> lookup(CP) == 'PVALID'.\n").

gen_lookup(Fd, Data) ->
  lists:foreach(fun({Cp, Class}) ->
    io:format(Fd, "lookup~s ~p;~n", [gen_single_clause(Cp), Class])
                end,
    optimize_ranges(lists:sort(Data))),
  io:put_chars(Fd, "lookup(_) -> 'UNASSIGNED'."),
  ok.

gen_single_clause({R0, undefined}) ->
  io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
  io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0,R1]).

optimize_ranges(Rs0) ->
  PF = fun
         ({{N, undefined}, _}) when is_integer(N) -> true;
         (_) -> false
       end,

  {Singles, Rs} = lists:partition(PF, Rs0),
  Singles ++ Rs.


to_range(CodePoints0) ->
  case tokens(CodePoints0, ".") of
    [CodePoint] ->
      {hex_to_int(CodePoint), undefined};
    [CodePoint1, "", CodePoint2] ->
      {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
  end.

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
  list_to_integer(?trim(HexStr), 16).

to_atom(Str) ->
  list_to_atom(?trim(Str)).

foldl(Fun, Acc, Fd) ->
  Get = fun() -> file:read_line(Fd) end,
  foldl_1(Fun, Acc, Get).

foldl_1(_Fun, {done, Acc}, _Get) -> Acc;
foldl_1(Fun, Acc, Get) ->
  case Get() of
    eof -> Acc;
    {ok, "#" ++ _} -> %% Ignore comments
      foldl_1(Fun, Acc, Get);
    {ok, "\n"} -> %% Ignore empty lines
      foldl_1(Fun, Acc, Get);
    {ok, Line} ->
      foldl_1(Fun, Fun(Line, Acc), Get)
  end.



%% Differs from string:tokens, it returns empty string as token between two delimiters
tokens(S, [C]) ->
  tokens(lists:reverse(S), C, []).

tokens([Sep|S], Sep, Toks) ->
  tokens(S, Sep, [[]|Toks]);
tokens([C|S], Sep, Toks) ->
  tokens_2(S, Sep, Toks, [C]);
tokens([], _, Toks) ->
  Toks.

tokens_2([Sep|S], Sep, Toks, Tok) ->
  tokens(S, Sep, [Tok|Toks]);
tokens_2([C|S], Sep, Toks, Tok) ->
  tokens_2(S, Sep, Toks, [C|Tok]);
tokens_2([], _Sep, Toks, Tok) ->
  [Tok|Toks].
