-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(138, length(Certs))
    ,?_assertMatch(<<48,130,6,75,48,130,4,51,160,3,2,1,2,2,17,0,217,181,67,127,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,5,207,48,130,3,183,160,3,2,1,2,2,20,8,22,95,138,76,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,2,43,48,130,1,177,160,3,2,1,2,2,10,123,113,182,130, _/binary>>, Cert3)
    ,?_assertMatch(<<48,130,3,117,48,130,2,93,160,3,2,1,2,2,11,4,0,0,0,0,1,21,75,90,195,148,48,13,6,_/binary>>, lists:last(Certs))
    ].
