%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
-module(punycode_test).
-author("benoitc").

-export([punicode_encode_test/0, punicode_decode_test/0]).

-include_lib("eunit/include/eunit.hrl").

data() ->
  [
    {"(A) Arabic (Egyptian)",
      [16#0644, 16#064A, 16#0647, 16#0645, 16#0627, 16#0628, 16#062A, 16#0643,
        16#0644, 16#0645, 16#0648, 16#0634, 16#0639, 16#0631, 16#0628, 16#064A,
        16#061F],
      "egbpdaj6bu4bxfgehfvwxn"
    },

    {
      "(B) Chinese (simplified)",
      [16#4ED6, 16#4EEC, 16#4E3A, 16#4EC0, 16#4E48, 16#4E0D, 16#8BF4, 16#4E2D,
        16#6587],
      "ihqwcrb4cv8a8dqg056pqjye"

    },
    {
      "(C) Chinese (traditional)",
      [16#4ED6, 16#5011, 16#7232, 16#4EC0, 16#9EBD, 16#4E0D, 16#8AAA, 16#4E2D,
        16#6587],
      "ihqwctvzc91f659drss3x8bo0yb"
    },
    {
      "(D) Czech: Pro<ccaron>prost<ecaron>nemluv<iacute><ccaron>esky",
      [16#0050, 16#0072, 16#006F, 16#010D, 16#0070, 16#0072, 16#006F, 16#0073,
        16#0074, 16#011B, 16#006E, 16#0065, 16#006D, 16#006C, 16#0075, 16#0076,
        16#00ED, 16#010D, 16#0065, 16#0073, 16#006B, 16#0079],
      "Proprostnemluvesky-uyb24dma41a"
    },
    {
      "(E) Hebrew:",
      [16#05DC, 16#05DE, 16#05D4, 16#05D4, 16#05DD, 16#05E4, 16#05E9, 16#05D5,
        16#05D8, 16#05DC, 16#05D0, 16#05DE, 16#05D3, 16#05D1, 16#05E8, 16#05D9,
        16#05DD, 16#05E2, 16#05D1, 16#05E8, 16#05D9, 16#05EA],
      "4dbcagdahymbxekheh6e0a7fei0b"
    },
    {
      "(F) Hindi (Devanagari):",
      [16#092F, 16#0939, 16#0932, 16#094B, 16#0917, 16#0939, 16#093F, 16#0928,
        16#094D, 16#0926, 16#0940, 16#0915, 16#094D, 16#092F, 16#094B, 16#0902,
        16#0928, 16#0939, 16#0940, 16#0902, 16#092C, 16#094B, 16#0932, 16#0938,
        16#0915, 16#0924, 16#0947, 16#0939, 16#0948, 16#0902],
      "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"
    },
    {
      "(G) Japanese (kanji and hiragana):",
      [16#306A, 16#305C, 16#307F, 16#3093, 16#306A, 16#65E5, 16#672C, 16#8A9E,
        16#3092, 16#8A71, 16#3057, 16#3066, 16#304F, 16#308C, 16#306A, 16#3044,
        16#306E, 16#304B],
      "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
    },
    {
      "(H) Korean (Hangul syllables):",
      [ 16#C138, 16#ACC4, 16#C758, 16#BAA8, 16#B4E0, 16#C0AC, 16#B78C, 16#B4E4,
        16#C774, 16#D55C, 16#AD6D, 16#C5B4, 16#B97C, 16#C774, 16#D574, 16#D55C,
        16#B2E4, 16#BA74, 16#C5BC, 16#B9C8, 16#B098, 16#C88B, 16#C744, 16#AE4C],
      "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5jpsd879ccm6fea98c"},
    {
      "(I) Russian (Cyrillic):",
      [16#043F, 16#043E, 16#0447, 16#0435, 16#043C, 16#0443, 16#0436, 16#0435,
        16#043E, 16#043D, 16#0438, 16#043D, 16#0435, 16#0433, 16#043E, 16#0432,
        16#043E, 16#0440, 16#044F, 16#0442, 16#043F, 16#043E, 16#0440, 16#0443,
        16#0441, 16#0441, 16#043A, 16#0438],
      "b1abfaaepdrnnbgefbadotcwatmq2g4l"
    },
    {
      "(J) Spanish: Porqu<eacute>nopuedensimplementehablarenEspa<ntilde>ol",
      [16#0050, 16#006F, 16#0072, 16#0071, 16#0075, 16#00E9, 16#006E, 16#006F,
        16#0070, 16#0075, 16#0065, 16#0064, 16#0065, 16#006E, 16#0073, 16#0069,
        16#006D, 16#0070, 16#006C, 16#0065, 16#006D, 16#0065, 16#006E, 16#0074,
        16#0065, 16#0068, 16#0061, 16#0062, 16#006C, 16#0061, 16#0072, 16#0065,
        16#006E, 16#0045, 16#0073, 16#0070, 16#0061, 16#00F1, 16#006F, 16#006C],
      "PorqunopuedensimplementehablarenEspaol-fmd56a"
    },
    {
      "(K) Vietnamese:",
      [16#0054, 16#1EA1, 16#0069, 16#0073, 16#0061, 16#006F, 16#0068, 16#1ECD,
        16#006B, 16#0068, 16#00F4, 16#006E, 16#0067, 16#0074, 16#0068, 16#1EC3,
        16#0063, 16#0068, 16#1EC9, 16#006E, 16#00F3, 16#0069, 16#0074, 16#0069,
        16#1EBF, 16#006E, 16#0067, 16#0056, 16#0069, 16#1EC7, 16#0074],
      "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"
    },
    {
      "(L) 3<nen>B<gumi><kinpachi><sensei>",
      [16#0033, 16#5E74, 16#0042, 16#7D44, 16#91D1, 16#516B, 16#5148, 16#751F],
      "3B-ww4c5e180e575a65lsy2b"
    },
    {
      "(M) <amuro><namie>-with-SUPER-MONKEYS",
      [16#5B89, 16#5BA4, 16#5948, 16#7F8E, 16#6075, 16#002D, 16#0077, 16#0069,
        16#0074, 16#0068, 16#002D, 16#0053, 16#0055, 16#0050, 16#0045, 16#0052,
        16#002D, 16#004D, 16#004F, 16#004E, 16#004B, 16#0045, 16#0059, 16#0053],
      "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"
    },
    {
      "(N) Hello-Another-Way-<sorezore><no><basho>",
      [16#0048, 16#0065, 16#006C, 16#006C, 16#006F, 16#002D, 16#0041, 16#006E,
        16#006F, 16#0074, 16#0068, 16#0065, 16#0072, 16#002D, 16#0057, 16#0061,
        16#0079, 16#002D, 16#305D, 16#308C, 16#305E, 16#308C, 16#306E, 16#5834,
        16#6240],
      "Hello-Another-Way--fc4qua05auwb3674vfr0b"
    },
    {
      "(O) <hitotsu><yane><no><shita>2",
      [16#3072, 16#3068, 16#3064, 16#5C4B, 16#6839, 16#306E, 16#4E0B, 16#0032],
      "2-u9tlzr9756bt3uc0v"
    },
    {
      "(P) Maji<de>Koi<suru>5<byou><mae>",
      [16#004D, 16#0061, 16#006A, 16#0069, 16#3067, 16#004B, 16#006F, 16#0069,
        16#3059, 16#308B, 16#0035, 16#79D2, 16#524D],
      "MajiKoi5-783gue6qz075azm5e"
    },
    {
      "(Q) <pafii>de<runba>",
      [16#30D1, 16#30D5, 16#30A3, 16#30FC, 16#0064, 16#0065, 16#30EB, 16#30F3, 16#30D0],
      "de-jg4avhby1noc0d"
    },
    {
      "(R) <sono><supiido><de>",
      [16#305D, 16#306E, 16#30B9, 16#30D4, 16#30FC, 16#30C9, 16#3067],
      "d9juau41awczczp"
    },
    {
      "(S) -> $1.00 <-",
      [16#002D, 16#003E, 16#0020, 16#0024, 16#0031, 16#002E, 16#0030, 16#0030,
        16#0020, 16#003C, 16#002D],
      "-> $1.00 <--"
    }
  ].

punicode_encode_test() ->
  lists:foreach(
    fun({_Descr, Input, Expect}) ->
      ?assertEqual(Expect, punycode:encode(Input))
    end,
    data()
  ).

punicode_decode_test() ->
  lists:foreach(
    fun({_Descr, Expect, Input}) ->
      ?assertEqual(Expect, punycode:decode(Input))
    end,
    data()
  ).
