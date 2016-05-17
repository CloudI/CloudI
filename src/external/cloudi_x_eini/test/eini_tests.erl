-module(eini_tests).

-author('shino@accense.com').

-include_lib("eunit/include/eunit.hrl").

-import(eini, [parse/1]).

setup() ->
  ok.

teardown(_) ->
  ok.

empty_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, []}, parse("")),
    ?_assertEqual({ok, []}, parse("\n"))
   ]}.

one_section_title_only_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% comment only
    ?_assertEqual({ok, []},
                  parse(
                    ";"
                   )),
    ?_assertEqual({ok, []},
                  parse(
                    ";    "
                   )),
    ?_assertEqual({ok, []},
                  parse(
                    "; comment"
                   )),
    ?_assertEqual({ok, []},
                  parse(
                    "; comment in Japanese 日本語"
                   )),
    %% Title only
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "[title]\n"
                   )),
    %% Title only, but trailing spaces
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "[title]  \n"
                   )),
    %% Title only, but comment lines
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "; comment line\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "; comment line\n"
                    "; comment line 2\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "; comment line\n"
                    "; comment line 2\n"
                    "  \n"
                    "[title]\n"
                    "; comment after section title"
                   )),
    %% Title only, but preceding blank lines
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "  \n"
                    "\n"
                    "[title]\n"
                   )),
    %% Title only, but preceding blank lines and trailing spaces
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "\n"
                    "  \n"
                    "[title]\t\s\n"
                   )),
    %% Title only, but trailing blank lines
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "[title]\n"
                    "\n"
                    "  \n"
                    "\n"
                   )),
    %% Title only, but trailing spaces and trailing blank lines
    ?_assertEqual({ok, [
                        {title, []}
                       ]},
                  parse(
                    "[title]  \n"
                    "\n"
                    "\n"
                   ))
   ]}.

one_section_title_only_syntax_error_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% No ] char
    ?_assertMatch({error, {syntax_error, 1, _Reason}},
                  parse(
                    "[title\n"
                   ))
   ]}.

one_section_title_and_one_prop_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% Simple case
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]\n"
                    "key1=value1\n"
                   )),
    %% title has trailing spaces
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=value1\n"
                   )),
    %% Single blank line between title and a prop
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "\n"
                    "key1=value1\n"
                   )),
    %% Single comment line between title and a prop
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "; comment\n"
                    "key1=value1\n"
                   )),
    %% Single comment line after a prop
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=value1\n"
                    "; comment\n"
                   )),
    %% Multi blank lines between title and a prop
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "\n"
                    "    \n"
                    "\n"
                    "key1=value1\n"
                   )),
    %% Multi blank lines after a prop
    ?_assertEqual({ok, [
                        {title, 
                         [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=value1\n"
                    "\n"
                    "    \n"
                    "\n"
                   )),
    %% Multi blank lines between title and a prop and
    %% multi blank lines after a prop
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "\n"
                    "    \n"
                    "\n"
                    "key1=value1\n"
                    "\n"
                    "    \n"
                    "\n"
                   )),

    %% value has [ char
    ?_assertEqual({ok, [
                        {title, [{key1, <<"va[lue1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=va[lue1\n"
                   )),
    %% value has ] char
    ?_assertEqual({ok, [
                        {title, [{key1, <<"valu]e1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=valu]e1\n"
                   )),
    %% value has [ and ] chars
    ?_assertEqual({ok, [
                        {title, [{key1, <<"va[lu]e1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=va[lu]e1\n"
                   )),
    %% value has < and > chars
    ?_assertEqual({ok, [
                        {title, [{key1, <<"va<lu>e1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=va<lu>e1\n"
                   )),
    %% value has ; char
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1;continue">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=value1;continue\n"
                   )),
    %% key has preceding spaces
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "  key1=value1\n"
                   )),
    %% key has trailing spaces
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1   =value1\n"
                   )),
    %% key has preceding and trailing spaces
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "   key1   =value1\n"
                   )),
    %% value has preceding and trailing spaces
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1">>}]}
                       ]},
                  parse(
                    "[title]  \n"
                    "key1=  value1  \n"
                   )),
    %% value has characters which can not used in titles or keys
    ?_assertEqual({ok, [
                        {title, [{key1, <<"value1$% '""#!+*=@/:+">>}]}
                       ]},
                  parse(
                    "[title]\n"
                    "key1=value1$% '""#!+*=@/:+\n"
                   ))
   ]}.

one_section_title_and_two_props_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% Simple case
    ?_assertEqual({ok, [
                        {title,
                         [{key1, <<"value1">>},
                          {key2, <<"value2">>}]}
                       ]},
                  parse(
                    "[title]\n"
                    "key1=value1\n"
                    "key2=value2\n"
                   )),
    %% Blank lines
    ?_assertEqual({ok, [
                        {title,
                         [{key1, <<"value1">>},
                          {key2, <<"value2">>}]}
                       ]},
                  parse(
                    "[title]\n"
                    "\n"
                    "key1=value1\n"
                    "  \n"
                    "\n"
                    "key2=value2\n"
                    "\n"
                    "\n"
                   ))
   ]}.

two_section_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, [
                        {titleA, []},
                        {titleB, []}
                       ]},
                  parse(
                    "[titleA]\n"
                    "[titleB]\n"
                   )),
    ?_assertEqual({ok, [
                        {titleA,
                         [{keyA1, <<"valueA1">>}]},
                        {titleB,
                         [{keyB1, <<"valueB1">>}]}
                       ]},
                  parse(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[titleB]  \n"
                    "keyB1=valueB1\n"
                   ))
   ]}.

binary_two_section_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, [
                        {titleA, []},
                        {titleB, []}
                       ]},
                  parse(
                    "[titleA]\n"
                    "[titleB]\n"
                   )),
    ?_assertEqual({ok, [
                        {titleA,
                         [{keyA1, <<"valueA1">>}]},
                        {titleB,
                         [{keyB1, <<"valueB1">>}]}
                       ]},
                  parse(
                    <<"[titleA]\n"
                      "keyA1=valueA1\n"
                      "[titleB]  \n"
                      "keyB1=valueB1\n">>
                   ))
   ]}.

lex_error_title_test_() ->
  {setup,
   fun setup/0,
   fun teardown/ 1,
   [
    %% vertical tab in section title
    ?_assertMatch({error, {illegal_character, 1, _Reason}},
                  parse("[ti\vtle]")),
    ?_assertMatch({error, {illegal_character, 3, _Reason}},
                  parse(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[tit\vleB]  \n"
                    "keyB1=valueB1\n"
                   ))
   ]}.
  
syntax_error_title_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% blank char before section title
    ?_assertMatch({error, {syntax_error, 1, ["syntax error before: ", _]}},
                  parse("  [title]")),
    %% blank char before section title, with a preceding empty line
    ?_assertMatch({error, {syntax_error, 2, ["syntax error before: ", _]}},
                  parse("\n"
                        "  [title]")),
    %% blank char before section title, with preceding empty lines
    ?_assertMatch({error, {syntax_error, 3, ["syntax error before: ", _]}},
                  parse("\n"
                        "\n"
                        "  [title]")),
    %% blank char before section title, with preceding blank lines
    ?_assertMatch({error, {syntax_error, 3, ["syntax error before: ", _]}},
                  parse("  \n"
                        "\t\n"
                        "  [title]")),
    %% blank char before section title, with preceding comment lines
    ?_assertMatch({error, {syntax_error, 4, ["syntax error before: ", _]}},
                  parse("; comment 1\n"
                        ";\n"
                        "; comment 2\n"
                        "  [title]")),
    %% blank char in section title
    ?_assertMatch({error, {syntax_error, 3, _Reason}},
                  parse(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[tit  leB]\n"
                    "keyB1=valueB1\n"
                   )),
    %% comment after title
    ?_assertMatch({error, {syntax_error, 1, ["syntax error before: ", _]}},
                  parse("[title] ;comment")),
    %% comment after blank
    ?_assertMatch({error, {syntax_error, 2, ["syntax error before: ", _]}},
                  parse("[title]\n"
                        " ;comment\n"))
   ]}.
  
syntax_error_property_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% blank char in key
    ?_assertMatch({error, {syntax_error, 2, _Reason}},
                  parse(
                    "[title]\n"
                    "key with blank=value\n"
                   )),
    %% comment after blank
    ?_assertMatch({error, {syntax_error, 2, ["syntax error before: ", _]}},
                  parse("[title]\n"
                        "key;comment=value\n"))
   ]}.
  
dup_title_test_() ->
  {setup,
   fun setup/0,
   fun teardown/ 1,
   [
    ?_assertEqual({error, {duplicate_title, titleA}},
                  parse(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[titleA]  \n"
                    "keyB1=valueB1\n"
                   ))
   ]}.
  
dup_key_test_() ->
  {setup,
   fun setup/0,
   fun teardown/ 1,
   [
    ?_assertEqual({error, {duplicate_key, titleB, key2}},
                  parse(
                    "[titleA]\n"
                    "key1=value1\n"
                    "[titleB]  \n"
                    "key1=value1\n"
                    "key2=value2\n"
                    "key3=value3\n"
                    "key2=value4\n"
                   )),
    ?_assertEqual({error, {duplicate_key, titleB, key2}},
                  parse(
                    "[titleA]\n"
                    "key1=value1\n"
                    "[titleB]  \n"
                    "key1=value1\n"
                    "key2=value2\n"
                    "key3=value3\n"
                    "key2  =  value4\n"
                   ))
   ]}.

register_test_() ->
  {foreach,
    fun() ->
      application:start(eini)
    end,
    fun(_) ->
      application:stop(eini)
    end,
    [
      {"syntax Error",
        ?_assertMatch({error, {syntax_error, 1, _Reason}},
                       eini:register("spam.ini" ,"[title\n"))},
      {"",
        fun() ->
          ?assertEqual(ok,
                       eini:register("spam.ini", "[title]\nkey=value")),
          ?assertEqual(<<"value">>,
                       eini:lookup_value("spam.ini", title, key)),
          ?assertEqual(not_found,
                       eini:lookup_value("spam.ini", title, key1)),
          ?assertEqual(ok,
                       eini:register("spam.ini", title, key1, <<"value">>)),
          ?assertEqual(<<"value">>,
                       eini:lookup_value("spam.ini", title, key1)),
          ?assertEqual({error, {duplicate_key, title, key1}},
                       eini:register("spam.ini", title, key1, <<"value">>)),
          ?assertEqual({error, {duplicate_key, title, key}},
                       eini:register("spam.ini", "[title]\nkey=value"))
        end}
    ]
  }.

is_section_test_() ->
  {foreach,
    fun() ->
      application:start(eini)
    end,
    fun(_) ->
      application:stop(eini)
    end,
    [
      {"",
        fun() ->
          ?assertEqual(ok,
                       eini:register("spam.ini", "[title]\nkey=value")),
          ?assertEqual(true,
                       eini:is_section("spam.ini", title)),
          ?assertEqual(false,
                       eini:is_section("spam.ini", title1))
        end}
    ]
  }.
