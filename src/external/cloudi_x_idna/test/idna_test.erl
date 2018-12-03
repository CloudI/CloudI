%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Aug 2018 11:28
%%%-------------------------------------------------------------------
-module(idna_test).
-author("benoitc").


-define(tld_strings, [
  {[16#6d4b,16#8bd5], "xn--0zwm56d"},
  {[16#092a,16#0930,16#0940,16#0915,16#094d,16#0937,16#093e], "xn--11b5bs3a9aj6g"},
  {[16#d55c,16#ad6d], "xn--3e0b707e"},
  {[16#09ad,16#09be,16#09b0,16#09a4], "xn--45brj9c"},
  {[16#09ac,16#09be,16#0982,16#09b2,16#09be], "xn--54b7fta0cc"},
  {[16#0438,16#0441,16#043f,16#044b,16#0442,16#0430,16#043d,16#0438,16#0435], "xn--80akhbyknj4f"},
  {[16#0441,16#0440,16#0431], "xn--90a3ac"},
  {[16#d14c,16#c2a4,16#d2b8], "xn--9t4b11yi5a"},
  {[16#0b9a,16#0bbf,16#0b99,16#0bcd,16#0b95,16#0baa,16#0bcd,16#0baa,16#0bc2,16#0bb0,16#0bcd], "xn--clchc0ea0b2g2a9gcd"},
  {[16#05d8,16#05e2,16#05e1,16#05d8], "xn--deba0ad"},
  {[16#4e2d,16#56fd], "xn--fiqs8s"},
  {[16#4e2d,16#570b], "xn--fiqz9s"},
  {[16#0c2d,16#0c3e,16#0c30,16#0c24,16#0c4d], "xn--fpcrj9c3d"},
  {[16#0dbd,16#0d82,16#0d9a,16#0dcf], "xn--fzc2c9e2c"},
  {[16#6e2c,16#8a66], "xn--g6w251d"},
  {[16#0aad,16#0abe,16#0ab0,16#0aa4], "xn--gecrj9c"},
  {[16#092d,16#093e,16#0930,16#0924], "xn--h2brj9c"},
  {[16#0622,16#0632,16#0645,16#0627,16#06cc,16#0634,16#06cc], "xn--hgbk6aj7f53bba"},
  {[16#0baa,16#0bb0,16#0bbf,16#0b9f,16#0bcd,16#0b9a,16#0bc8], "xn--hlcj6aya9esc7a"},
  {[16#0443,16#043a,16#0440], "xn--j1amh"},
  {[16#9999,16#6e2f], "xn--j6w193g"},
  {[16#03b4,16#03bf,16#03ba,16#03b9,16#03bc,16#03ae], "xn--jxalpdlp"},
  {[16#0625,16#062e,16#062a,16#0628,16#0627,16#0631], "xn--kgbechtv"},
  {[16#53f0,16#6e7e], "xn--kprw13d"},
  {[16#53f0,16#7063], "xn--kpry57d"},
  {[16#0627,16#0644,16#062c,16#0632,16#0627,16#0626,16#0631], "xn--lgbbat1ad8j"},
  {[16#0639,16#0645,16#0627,16#0646], "xn--mgb9awbf"},
  {[16#0627,16#06cc,16#0631,16#0627,16#0646], "xn--mgba3a4f16a"},
  {[16#0627,16#0645,16#0627,16#0631,16#0627,16#062a], "xn--mgbaam7a8h"},
  {[16#067e,16#0627,16#06a9,16#0633,16#062a,16#0627,16#0646], "xn--mgbai9azgqp6j"},
  {[16#0627,16#0644,16#0627,16#0631,16#062f,16#0646], "xn--mgbayh7gpa"},
  {[16#0628,16#06be,16#0627,16#0631,16#062a], "xn--mgbbh1a71e"},
  {[16#0627,16#0644,16#0645,16#063a,16#0631,16#0628], "xn--mgbc0a9azcg"},
  {[16#0627,16#0644,16#0633,16#0639,16#0648,16#062f,16#064a,16#0629], "xn--mgberp4a5d4ar"},
  {[16#10d2,16#10d4], "xn--node"},
  {[16#0e44,16#0e17,16#0e22], "xn--o3cw4h"},
  {[16#0633,16#0648,16#0631,16#064a,16#0629], "xn--ogbpf8fl"},
  {[16#0440,16#0444], "xn--p1ai"},
  {[16#062a,16#0648,16#0646,16#0633], "xn--pgbs0dh"},
  {[16#0a2d,16#0a3e,16#0a30,16#0a24], "xn--s9brj9c"},
  {[16#0645,16#0635,16#0631], "xn--wgbh1c"},
  {[16#0642,16#0637,16#0631], "xn--wgbl6a"},
  {[16#0b87,16#0bb2,16#0b99,16#0bcd,16#0b95,16#0bc8], "xn--xkc2al3hye2a"},
  {[16#0b87,16#0ba8,16#0bcd,16#0ba4,16#0bbf,16#0baf,16#0bbe], "xn--xkc2dl3a5ee0h"},
  {[16#65b0,16#52a0,16#5761], "xn--yfro4i67o"},
  {[16#0641,16#0644,16#0633,16#0637,16#064a,16#0646], "xn--ygbi2ammx"},
  {[16#30c6,16#30b9,16#30c8], "xn--zckzah"},
  {[16#049b,16#0430,16#0437], "xn--80ao21a"},
  {[16#0645,16#0644,16#064a,16#0633,16#064a,16#0627], "xn--mgbx4cd0ab"},
  {[16#043c,16#043e,16#043d], "xn--l1acc"},
  {[16#0633,16#0648,16#062f,16#0627,16#0646], "xn--mgbpl2fh"}
]).

-include_lib("eunit/include/eunit.hrl").

alabels_test() ->
  lists:foreach(
    fun({ULabel, ALabel}) ->
      ?assertEqual(ALabel, idna:alabel(ULabel))
    end,
    ?tld_strings
  ).

ulabels_test() ->
  lists:foreach(
    fun({ULabel, ALabel}) ->
      ?assertEqual(ULabel, idna:ulabel(ALabel))
    end,
    ?tld_strings
  ).

check_label_length_test() ->
  ?assertEqual(ok, idna:check_label_length([$a || _ <- lists:seq(1, 63)])),
  ?assertExit({bad_label, {too_long, _Error}}, idna:check_label_length([$a || _ <- lists:seq(1, 64)])),
  ?assertExit({bad_label, {too_long, _Error}}, idna:encode([$a || _ <- lists:seq(1, 64)])).

check_bidi_test() ->
  L = [16#0061],
  R = [16#05d0],
  AL= [16#0627],
  AN = [16#0660],
  EN = [16#0030],
  ES = [16#002d],
  CS = [16#002c],
  ET = [16#0024],
  ON = [16#0021],
  BN = [16#200c],
  NSM = [16#0610],
  WS = [16#0020],

  %% RFC 5893 Rule 1
  ok = idna_bidi:check_bidi(L),
  ok = idna_bidi:check_bidi(R),
  ok = idna_bidi:check_bidi(AL),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(AN)),

  %% RFC 5893 Rule 2
  ok = idna_bidi:check_bidi(R ++ AL),
  ok = idna_bidi:check_bidi(R ++ AN),
  ok = idna_bidi:check_bidi(R ++ EN),
  ok = idna_bidi:check_bidi(R ++ ES ++ AL),
  ok = idna_bidi:check_bidi(R ++ CS ++ AL),
  ok = idna_bidi:check_bidi(R ++ ET ++ AL),
  ok = idna_bidi:check_bidi(R ++ ON ++ AL),
  ok = idna_bidi:check_bidi(R ++ BN ++ AL),
  ok = idna_bidi:check_bidi(R ++ NSM),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(R ++ L)),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(R ++ WS)),

  %% RFC 5893 Rule 3
  ok = idna_bidi:check_bidi(R ++ AL),
  ok = idna_bidi:check_bidi(R ++ EN),
  ok = idna_bidi:check_bidi(R ++ AN),
  ok = idna_bidi:check_bidi(R ++ NSM),
  ok = idna_bidi:check_bidi(R ++ NSM ++ NSM),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(R ++ ON)),

  %% RFC 5893 Rule 4
  ok = idna_bidi:check_bidi(R ++ EN),
  ok = idna_bidi:check_bidi(R ++ AN),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(R ++ EN ++ AN)),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(R ++ AN ++ EN)),

  %% RFC 5893 Rule 5
  ok = idna_bidi:check_bidi(L ++ EN, true),
  ok = idna_bidi:check_bidi(L ++ ES ++ L, true),
  ok = idna_bidi:check_bidi(L ++ CS ++ L, true),
  ok = idna_bidi:check_bidi(L ++ ET ++ L, true),
  ok = idna_bidi:check_bidi(L ++ ON ++ L, true),
  ok = idna_bidi:check_bidi(L ++ BN ++ L, true),
  ok = idna_bidi:check_bidi(L ++ NSM, true),

  %% RFC 5893 Rule 6
  ok = idna_bidi:check_bidi(L ++ L, true),
  ok = idna_bidi:check_bidi(L ++ EN, true),
  ok = idna_bidi:check_bidi(L ++ EN ++ NSM, true),
  ok = idna_bidi:check_bidi(L ++ EN ++ NSM ++ NSM, true),
  ?assertExit({bad_label, {bidi, _}}, idna_bidi:check_bidi(L ++ CS, true)).

check_initial_combiner_test() ->
  M = [16#0300],
  A = [16#0061],

  ok = idna:check_initial_combiner(A),
  ok = idna:check_initial_combiner(A ++ M),
  ?assertExit({bad_label, {initial_combiner, _}},idna:check_initial_combiner(M ++ A)).

check_hyphen_test() ->
  ok  = idna:check_hyphen("abc"),
  ok  = idna:check_hyphen("a--b"),
  ?assertExit({bad_label, {hyphen, _}},idna:check_hyphen("aa--")),
  ?assertExit({bad_label, {hyphen, _}},idna:check_hyphen("a-")),
  ?assertExit({bad_label, {hyphen, _}},idna:check_hyphen("-a")).


valid_contextj_test() ->
  Zwnj = [16#200c],
  Zwj = [16#200d],
  Virama = [16#094d],
  Latin = [16#0061],

  % RFC 5892 Appendix A.1 (Zero Width Non-Joiner)
  false = idna_context:valid_contextj(Zwnj, 0),
  false = idna_context:valid_contextj(Latin ++ Zwnj, 1),
  true = idna_context:valid_contextj(Virama ++ Zwnj, 1),

  % RFC 5892 Appendix A.2 (Zero Width Joiner)
  false = idna_context:valid_contextj(Zwj, 0),
  false = idna_context:valid_contextj(Latin ++ Zwj, 1),
  true = idna_context:valid_contextj(Virama ++ Zwj, 1).


valid_contexto_test() ->
  Latin = [16#0061],
  Latin_l = [16#006c],
  Greek = [16#03b1],
  Hebrew = [16#05d0],
  Katakana = [16#30a1],
  Hiragana = [16#3041],
  Han = [16#6f22],
  Arabic_digit = [16#0660],
  Ext_arabic_digit = [16#06f0],

  % RFC 5892 Rule A.3 (Middle Dot)
  Latin_middle_dot = [16#00b7],
  true = idna_context:valid_contexto(Latin_l ++ Latin_middle_dot ++  Latin_l, 1),
  false = idna_context:valid_contexto(Latin_middle_dot ++ Latin_l, 1),
  false = idna_context:valid_contexto(Latin_l ++ Latin_middle_dot, 0),
  false = idna_context:valid_contexto(Latin_middle_dot, 0),
  false = idna_context:valid_contexto(Latin_l ++ Latin_middle_dot ++ Latin, 1),

  % RFC 5892 Rule A.4 (Greek Lower Numeral Sign)
  Glns = [16#0375],
  true = idna_context:valid_contexto(Glns ++ Greek, 0),
  false = idna_context:valid_contexto(Glns ++ Latin, 0),
  false = idna_context:valid_contexto(Glns, 0),
  false = idna_context:valid_contexto(Greek ++ Glns, 1),

  % RFC 5892 Rule A.5 (Hebrew Punctuation Geresh)
  Geresh = [16#05f3],
  true = idna_context:valid_contexto(Hebrew ++ Geresh, 1),
  false = idna_context:valid_contexto(Latin ++ Geresh, 1),

  % RFC 5892 Rule A.6 (Hebrew Punctuation Gershayim)
  Gershayim = [16#05f4],
  true = idna_context:valid_contexto(Hebrew ++ Gershayim, 1),
  false = idna_context:valid_contexto(Latin ++ Gershayim, 1),

  % RFC 5892 Rule A.7 (Katakana Middle Dot)
  Ja_middle_dot = [16#30fb],
  true = idna_context:valid_contexto(Katakana ++ Ja_middle_dot ++ Katakana, 1),
  true = idna_context:valid_contexto(Hiragana ++ Ja_middle_dot ++ Hiragana, 1),
  true = idna_context:valid_contexto(Han ++ Ja_middle_dot ++ Han, 1),
  true = idna_context:valid_contexto(Han ++ Ja_middle_dot ++ Latin, 1),
  true = idna_context:valid_contexto([16#6f22, 16#30fb, 16#5b57], 1),
  false = idna_context:valid_contexto([16#0061, 16#30fb, 16#0061], 1),

  % RFC 5892 Rule A.8 (Arabic-Indic Digits)
  true = idna_context:valid_contexto(Katakana ++ Ja_middle_dot ++ Katakana, 1),
  false = idna_context:valid_contexto([16#0061, 16#30fb, 16#0061], 1),

  % RFC 5892 Rule A.9 (Extended Arabic-Indic Digits)
  true = idna_context:valid_contexto(Ext_arabic_digit ++ Ext_arabic_digit, 0),
  false = idna_context:valid_contexto(Ext_arabic_digit ++ Arabic_digit, 0).

encode_test() ->
  ?assertEqual("xn--zckzah.xn--zckzah", idna:encode("xn--zckzah.xn--zckzah")),
  ?assertEqual("xn--zckzah.xn--zckzah", idna:encode([16#30c6,16#30b9,16#30c8, $., 16#30c6, 16#30b9, 16#30c8])),
  ?assertEqual("abc.abc", idna:encode("abc.abc")),
  ?assertEqual("xn--zckzah.abc", idna:encode("xn--zckzah.abc")),
  ?assertEqual("xn--zckzah.abc", idna:encode([16#30c6, 16#30b9, 16#30c8, $., $a, $b, $c])),
  ?assertEqual(
    "xn---------90gglbagaar.aa",
    idna:encode([16#0521,16#0525,16#0523,$-,16#0523,16#0523,$-,$-,$-,$-,$-,16#0521,16#0523,16#0523,16#0523|".aa"])
  ),
  ?assertExit(
    {bad_label, {_, _}},
    idna:encode(
      [16#0521,16#0524,16#0523,$-,16#0523,16#0523,$-,$-,$-,$-,$-,16#0521,16#0523,16#0523,16#0523|".aa"],
      [{uts46, false}]
    )
  ),
  ?assertEqual([$a ||_ <- lists:seq(1, 63)], idna:encode([$a ||_ <- lists:seq(1, 63)])),
  ?assertExit({bad_label, {_, _}}, idna:encode([$a ||_ <- lists:seq(1, 64)])).


decode_test() ->
  ?assertEqual([16#30c6, 16#30b9, 16#30c8, $., 16#30c6, 16#30b9, 16#30c8], idna:decode("xn--zckzah.xn--zckzah")),
  ?assertEqual(
    [16#30c6, 16#30b9, 16#30c8, $., 16#30c6, 16#30b9, 16#30c8],
    idna:decode([16#30c6, 16#30b9, 16#30c8|".xn--zckzah"])
  ),
  ?assertEqual(
    [16#30c6, 16#30b9, 16#30c8, $., 16#30c6, 16#30b9, 16#30c8],
    idna:decode([16#30c6, 16#30b9, 16#30c8, $., 16#30c6, 16#30b9, 16#30c8])
  ),
  ?assertEqual("abc.abc", idna:decode("abc.abc")),
  ?assertEqual(
    [16#0521,16#0525,16#0523,$-,16#0523,16#0523,$-,$-,$-,$-,$-,16#0521,16#0523,16#0523,16#0523|".aa"],
    idna:decode("xn---------90gglbagaar.aa")
  ),
  ?assertExit({bad_label, {_, _}}, idna:decode("XN---------90GGLBAGAAC.AA")),
  ?assertExit({bad_label, {_, _}}, idna:decode("xn---------90gglbagaac.aa")).