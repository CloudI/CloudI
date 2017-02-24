-module(idna_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

fixture_path(Name) ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppPath = filename:dirname(EbinDir),
    filename:join([AppPath, "test", "fixtures", Name]).


punycode_encode_test() ->
    test_each(fixture_path("punycode_*"), fun (Test) ->
                                                  Expect = proplists:get_value(punycode, Test),
                                                  Input =proplists:get_value(unicode, Test),
                                                  ?assertEqual(Expect, punycode:encode(Input))
                                          end).

idna_to_ascii_test() ->
    test_each(fixture_path("idna_*"), fun (Test) ->
                                     Expect = proplists:get_value(output, Test),
                                     Input = proplists:get_value(input, Test),
                                     ?assertEqual(Expect,idna:to_ascii(idna_ucs:from_utf8(Input)))
                             end).

test_each(FilePattern, Fun) ->
    test_each1(filelib:wildcard(FilePattern), Fun).

test_each1([], _) ->
    ok;
test_each1([File | Files], Fun) ->
    io:format("file is ~p~n", [File]),
    {ok, Test} = file:consult(File),
    Fun(Test),
    test_each1(Files, Fun).

