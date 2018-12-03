#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_data").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str, Dir), string:trim(Str, Dir)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-define(lower(C), string:lowercase(C)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str, Dir), string:strip(Str, Dir)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-define(lower(C), string:to_lower(C)).
-endif.

%% Default `Bidi_Class` for unassigned codepoints.
%%
%% Ref: <https://www.unicode.org/Public/UNIDATA/extracted/DerivedBidiClass.txt>
-define(BIDI_CLASS_DEFAULTS, [
  {{16#0600, 16#07BF}, "AL"},
  {{16#0860, 16#086F}, "AL"},
  {{16#08A0, 16#08FF}, "AL"},
  {{16#FB50, 16#FDCF}, "AL"},
  {{16#FDF0, 16#FDFF}, "AL"},
  {{16#FE70, 16#FEFF}, "AL"},
  {{16#00010D00, 16#00010D3F}, "AL"},
  {{16#00010F30, 16#00010F6F}, "AL"},
  {{16#0001EC70, 16#0001ECBF}, "AL"},
  {{16#0001EE00, 16#0001EEFF}, "AL"},
  %% Arabic, Syriac, and Thaana blocks, among others
  {{16#0590, 16#05FF}, "R"},
  {{16#07C0, 16#085F}, "R"},
  {{16#0870, 16#089F}, "R"},
  {{16#FB1D, 16#FB4F}, "R"},
  {{16#00010800, 16#00010CFF}, "R"},
  {{16#00010D40, 16#00010F2F}, "R"},
  {{16#00010F70, 16#00010FFF}, "R"},
  {{16#0001E800, 16#0001EC6F}, "R"},
  {{16#0001ECC0, 16#0001EDFF}, "R"},
  {{16#0001EF00, 16#0001EFFF}, "R"},
  %% Hebrew, NKo, and Phoenician blocks, among others.
  {{16#20A0, 16#20CF}, "ET"}
  %% Currency Symbols block.
]).

main(_) ->
  {ok, IM} = file:open("../uc_spec/UnicodeData.txt", [read, raw, {read_ahead, 1000000}]),
  Data = foldl(fun parse_unicode_data/2, [], IM),
  file:close(IM),

  {ok, AS} = file:open("../uc_spec/ArabicShaping.txt", [read, raw, {read_ahead, 1000000}]),
  JoiningTypes = foldl(fun parse_as/2, [], AS),
  ok = file:close(AS),

  {ok, ScriptsF} = file:open("../uc_spec/Scripts.txt", [read, raw, {read_ahead, 1000000}]),
  Scripts = foldl(fun parse_scripts/2, [], ScriptsF),
  ok = file:close(ScriptsF),

  %% Make module
  OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
  {ok, Out} = file:open(OutputPath, [write]),
  gen_file(Out, Data, JoiningTypes, Scripts),
  ok = file:close(Out),
  ok.

gen_file(Fd, Data, JoiningTypes, Scripts) ->
  gen_header(Fd),
  gen_bidirectional(Fd),
  gen_lookup(Fd, Data),
  gen_joining_types(Fd, JoiningTypes),
  gen_scripts_types(Fd, Scripts),
  ok.


gen_header(Fd) ->
  io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
  io:put_chars(Fd, "%% see ../uc_spec/gen_idnadata_mod.escript\n\n"),
  io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
  io:put_chars(Fd, "-compile(compressed).\n"),
  io:put_chars(Fd, "-export([lookup/1, joining_types/1, scripts/1]).\n"),
  io:put_chars(Fd, "-export([bidirectional/1]).\n"),
  ok.

gen_lookup(Fd, Data) ->
  lists:foreach(
    fun({Cp,Tp}) ->
      io:format(Fd, "lookup(~w) -> ~p;~n", [Cp, Tp])
    end,
    lists:sort(Data)
  ),
  io:put_chars(Fd, "lookup(_) -> false.\n\n").

gen_bidirectional(Fd) ->
  io:put_chars(Fd, "bidirectional(CP) ->\n"),
  io:put_chars(Fd, "  case lookup(CP) of \n"),
  io:put_chars(Fd, "    {_, C} -> C;\n"),
  io:put_chars(Fd, "    false -> bidirectional_1(CP)\n"),
  io:put_chars(Fd, "  end.\n\n"),
  lists:foreach(
    fun({Cp, Class}) ->
      io:format(Fd, "bidirectional_1~s ~p;~n", [gen_single_clause(Cp), Class])
    end,
    lists:sort(?BIDI_CLASS_DEFAULTS)
  ),
  io:put_chars(Fd, "bidirectional_1(_) -> \"L\".\n\n").

gen_joining_types(Fd, JoiningTypes) ->
  lists:foreach(
    fun({Cp, Jt}) ->
      io:format(Fd, "joining_types(~w) -> ~p;~n", [Cp, ?trim(Jt, both)])
    end,
    lists:sort(JoiningTypes)
  ),
  io:put_chars(Fd, "joining_types(_) -> undefined.\n\n").

gen_scripts_types(Fd, Scripts) ->
  lists:foreach(
    fun({Cp, Jt}) ->
      io:format(Fd, "scripts~s ~p;~n", [gen_single_clause(Cp), ?lower(?trim(Jt, both))])
    end,
    optimize_scripts_ranges(lists:sort(Scripts))
  ),
  io:put_chars(Fd, "scripts(_) -> false.\n\n").

optimize_scripts_ranges(Rs0) ->
  PF = fun
         ({{N, undefined}, _}) when is_integer(N) -> true;
         (_) -> false
       end,

  {Singles, Rs} = lists:partition(PF, Rs0),
  Singles ++ Rs.


gen_single_clause({R0, undefined}) ->
  io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
  io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0,R1]).



parse_unicode_data(Line0, Acc) ->
  Line = ?chomp(Line0),
  [CodePoint, _Name, Cat, _Class, BiDi |_] = tokens(Line, ";"),
  [{hex_to_int(CodePoint), {?trim(Cat, both), ?trim(BiDi, both)}} | Acc].

parse_as(Line0, Acc) ->
  Line = ?chomp(Line0),
  case tokens(Line, ";") of
    [CodePoint, _,JT|_] ->
      [{hex_to_int(CodePoint), ?trim(JT, both) } | Acc];
    _ ->
      Acc
  end.

parse_scripts(Line0, Acc) ->
  [Line|_Comments] = tokens(Line0, "#"),
  [CodePoints, Script0] = tokens(Line, ";"),
  Script1 = ?trim(Script0, both),
  case lists:member(Script1, ["Greek", "Han", "Hebrew", "Hiragana", "Katakana"]) of
    true ->
      [{to_range(CodePoints), Script1} | Acc];
    false ->
      Acc
  end.


to_range(CodePoints0) ->
  case tokens(CodePoints0, ".") of
    [CodePoint] ->
      {hex_to_int(CodePoint), undefined};
    [CodePoint1, "", CodePoint2] ->
      {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
  end.

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
  list_to_integer(?trim(HexStr, both), 16).


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
