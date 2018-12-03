#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_mapping").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str), string:trim(Str, both)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-define(lower(C), string:lowercase(C)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str), string:strip(Str, both)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-define(lower(C), string:to_lower(C)).
-endif.

-define(UTS46_STATUSES, #{
  "valid" => {'V', false},
  "ignored" => {'I', false},
  "mapped" => {'M', true},
  "deviation" => {'D', true},
  "disallowed" => {'X', false},
  "disallowed_STD3_valid" => {'3', false},
  "disallowed_STD3_mapped" => {'3', true}
}).



main(_) ->
  {ok, IM} = file:open("../uc_spec/IdnaMappingTable.txt", [read, raw, {read_ahead, 1000000}]),
  Data = foldl(fun parse_idna_mapping/2, [], IM),
  file:close(IM),

  %% Make module
  OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
  {ok, Out} = file:open(OutputPath, [write]),
  gen_file(Out, Data),
  ok = file:close(Out),
  ok.

parse_idna_mapping(Line0, Acc) ->
  [Line|_Comments] = tokens(Line0, "#"),
  case tokens(Line, ";") of
    [CodePoints, Status] ->
      [{to_range(CodePoints), {?trim(Status), undefined, undefined}} | Acc];
    [CodePoints, Status, Mapping] ->
      [{to_range(CodePoints), {?trim(Status), to_mapping(Mapping), undefined}} | Acc];
    [CodePoints, Status, Mapping, Idna2008Status] ->
      [{to_range(CodePoints), {?trim(Status), to_mapping(Mapping), to_atom(Idna2008Status)}} | Acc]
  end.


to_mapping(Mapping) ->
  [hex_to_int(C) || C <- ?lexemes(Mapping, " ")].

to_range(CodePoints0) ->
  case tokens(CodePoints0, ".") of
    [CodePoint] ->
      {hex_to_int(CodePoint), undefined};
    [CodePoint1, "", CodePoint2] ->
      {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
  end.


gen_file(Fd, Data) ->
  gen_header(Fd),
  gen_utc46(Fd, Data),
  ok.


gen_header(Fd) ->
  io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
  io:put_chars(Fd, "%% see ../uc_spec/gen_idna_mapping.escript\n\n"),
  io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
  io:put_chars(Fd, "-compile(compressed).\n"),
  io:put_chars(Fd, "-export([uts46_map/1]).\n"),
  ok.

gen_utc46(Fd, Data) ->
  lists:foreach(
    fun({CP, {S, M, _}}) ->
        {Status, Mapping} = maps:get(S, ?UTS46_STATUSES),
        case Mapping of
          true ->
            io:format(Fd, "uts46_map~s {~p, ~w};\n", [gen_single_clause(CP), Status, M]);
          false ->
            io:format(Fd, "uts46_map~s ~p;\n", [gen_single_clause(CP), Status])
        end
    end,
    optimize_ranges(lists:sort(Data))
  ),
  io:put_chars(Fd, "uts46_map(_) -> erlang:error(badarg).\n").


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
  list_to_integer(?trim(HexStr), 16).

to_atom(Str) ->
  list_to_atom(?lower(?trim(Str))).

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
