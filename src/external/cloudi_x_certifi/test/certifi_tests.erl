-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(132, length(Certs))
    ,?_assertMatch(<<48,130,2,148,48,130,2,26,160,3,2,1,2,2,8,44,41,156,91,22,237,5,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,5,235,48,130,3,211,160,3,2,1,2,2,8,86,182,41,205,52,188,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,2,141,48,130,2,20,160,3,2,1,2,2,8,117,230,223,203,193,_/binary>>, Cert3)
    ,?_assertMatch(<<48,130,3,117,48,130,2,93,160,3,2,1,2,2,11,4,0,0,0,0,1,21,75,90,195,148,48,13,6,_/binary>>, lists:last(Certs))
    ].
