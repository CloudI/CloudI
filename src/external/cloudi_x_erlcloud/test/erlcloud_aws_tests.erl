-module(erlcloud_aws_tests).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("eunit/include/eunit.hrl").

request_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun request_default_test/1,
      fun request_prot_host_port_str_test/1,
      fun request_prot_host_port_int_test/1,
      fun get_service_status_test/1]}.

start() ->
    meck:new(erlcloud_httpc),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], ok}} end),
    ok.

stop(_) ->
    meck:unload(erlcloud_httpc).

request_default_test(_) ->
    ok = erlcloud_aws:aws_request(get, "host", "/", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(https, "host", 443, "/", Url).

request_prot_host_port_str_test(_) ->
    ok = erlcloud_aws:aws_request(get, "http", "host1", "9999", "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).

request_prot_host_port_int_test(_) ->
    ok = erlcloud_aws:aws_request(get, "http", "host1", 9999, "/path1", [], "id", "key"),
    Url = get_url_from_history(meck:history(erlcloud_httpc)),
    test_url(http, "host1", 9999, "/path1", Url).

get_service_status_test(_) ->
    StatusJsonS3 = jsx:encode(
        [{<<"archive">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,1},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-us-standard">>}]
            ]},
         {<<"current">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,0},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-eu-central-1">>}],
             [{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,2},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"ec2-us-west-2">>}]
            ]}
        ]
    ),
    OKStatusEmptyJson = jsx:encode(
        [{<<"archive">>,
            [[{<<"service_name">>,
               <<"Amazon Simple Storage Service (US Standard)">>},
              {<<"summary">>,
               <<"[RESOLVED] Elevated errors for bucket operations in US-STANDARD ">>},
              {<<"date">>,<<"1408497982">>},
              {<<"status">>,1},
              {<<"details">>,<<>>},
              {<<"description">>,
               <<"<div><span class=\"yellowfg\"> 6:46 PM PDT</span>&nbsp;We are investigatin">>},
              {<<"service">>,<<"s3-us-standard">>}]
            ]},
         {<<"current">>,[]}
        ]
    ),

    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], StatusJsonS3}} end),
    [S3Status, EC2Status] = erlcloud_aws:get_service_status(["s3", "ec2", "sns"]),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], OKStatusEmptyJson}} end),
    OKStatusEmpty = erlcloud_aws:get_service_status(["sqs", "sns"]),
    meck:expect(erlcloud_httpc, request, fun(_,_,_,_,_,_) -> {ok, {{200, "OK"}, [], StatusJsonS3}} end),
    OKStatus = erlcloud_aws:get_service_status(["cloudformation", "sns", "vpc"]),
    
    [?_assertEqual(proplists:get_value(<<"status">>, S3Status), 0),
     ?_assertEqual(proplists:get_value(<<"service">>, S3Status), <<"s3-eu-central-1">>),
     ?_assertEqual(proplists:get_value(<<"status">>, EC2Status), 2),
     ?_assertEqual(proplists:get_value(<<"service">>, EC2Status), <<"ec2-us-west-2">>),
     ?_assertEqual(OKStatusEmpty, ok),
     ?_assertEqual(OKStatus, ok)
     ].

% ==================
% Internal functions
% ==================

get_url_from_history([{_, {erlcloud_httpc, request, [Url, _, _, _, _, _]}, _}]) ->
    Url.

test_url(ExpScheme, ExpHost, ExpPort, ExpPath, Url) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(Url),
    [?_assertEqual(ExpScheme, Scheme),
     ?_assertEqual(ExpHost, Host),
     ?_assertEqual(ExpPort, Port),
     ?_assertEqual(ExpPath, Path)].


profile_default_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX2",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2" }},
           erlcloud_aws:profile() )
       )
    }.


profile_direct_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX1",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1" }},
           erlcloud_aws:profile( bar ) )
       )
    }.

profile_indirect_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX1",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1" }},
           erlcloud_aws:profile( blah ) )
       )
    }.
    
profile_indirect_role_test_() ->
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:12345" }},
           erlcloud_aws:profile( flooga ) )
       )
    }.

profile_indirect_role_options_test_() ->
    Options = [{role_session_name, "wonder"},
               {role_duration_secs, 3600}],
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:54321" }},
           erlcloud_aws:profile( flooga, Options ) )
       )
    }.

profile_indirect_role_options_external_id_test_() ->
    Options = [{role_session_name, "wonder"},
               {role_duration_secs, 3600},
               {external_id, "HOOPDIE"}],
    {setup, fun profiles_assume_setup/0, fun profiles_assume_cleanup/1,
     ?_test(
        ?assertMatch(
           {ok, #aws_config{
            access_key_id = "XXXXXXXXXXXXXXXXXXX3",
            secret_access_key = "yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3",
            security_token = "WHOOOOOOOO:54321" }},
           erlcloud_aws:profile( flooga, Options ) )
       )
    }.

profile_undefined_profile_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch( {error, _}, erlcloud_aws:profile( what ) )
       )
    }.
    
profile_undefined_indirect_profile_test_() ->
    {setup, fun profiles_test_setup/0, fun profiles_test_cleanup/1,
     ?_test(
        ?assertMatch( {error, _}, erlcloud_aws:profile( whoa ) )
       )
    }.
    

profiles_test_setup() ->
    Profile = <<"
[bar]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX1
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy1

[baz]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX3
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy3

[flooga]
role_arn=arn:aws:iam::892406118791:role/centralized-users
source_profile=baz

[default]
aws_access_key_id = XXXXXXXXXXXXXXXXXXX2
aws_secret_access_key = yyyyyyyyyyyyyyyyyyyyyyyyyy+yyyy/yyyyyyyy2

[blah]
source_profile=bar

[whoa]
source_profile=cowboy
">>,
    meck:new( file, [unstick, passthrough] ),
    meck:expect( file, read_file, fun( _ ) -> {ok, Profile} end ).

profiles_test_cleanup(_) ->
    meck:unload( file ).

profiles_assume_setup() ->
    profiles_test_setup(),
    meck:new( erlcloud_sts ),
    meck:expect( erlcloud_sts, assume_role,
                 fun( Config, _, "erlcloud", 900, _ ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:12345" },
                       []};
                    ( Config, _, "wonder", 3600, _ ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:54321" },
                       []};
                    ( Config, _, "external", 3600, "HOOPDIE" ) ->
                      {Config#aws_config{ security_token = "WHOOOOOOOO:99999" },
                       []}
                 end ).

profiles_assume_cleanup(P) ->
    profiles_test_cleanup(P),
    meck:unload( erlcloud_sts ).


service_config_autoscaling_test() ->
    Service = <<"autoscaling">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["autoscaling.us-east-1.amazonaws.com",
                "autoscaling.us-west-1.amazonaws.com",
                "autoscaling.us-west-2.amazonaws.com",
                "autoscaling.eu-west-1.amazonaws.com",
                "autoscaling.eu-central-1.amazonaws.com",
                "autoscaling.ap-northeast-1.amazonaws.com",
                "autoscaling.ap-northeast-2.amazonaws.com",
                "autoscaling.ap-southeast-1.amazonaws.com",
                "autoscaling.ap-southeast-2.amazonaws.com",
                "autoscaling.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ as_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_cloudformation_test() ->
    Service = <<"cloudformation">>,
    ServiceAlt = <<"cfn">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["cloudformation.us-east-1.amazonaws.com",
                "cloudformation.us-west-1.amazonaws.com",
                "cloudformation.us-west-2.amazonaws.com",
                "cloudformation.eu-west-1.amazonaws.com",
                "cloudformation.eu-central-1.amazonaws.com",
                "cloudformation.ap-northeast-1.amazonaws.com",
                "cloudformation.ap-northeast-2.amazonaws.com",
                "cloudformation.ap-southeast-1.amazonaws.com",
                "cloudformation.ap-southeast-2.amazonaws.com",
                "cloudformation.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudformation_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudformation_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_cloudtrail_test() ->
    Service = <<"cloudtrail">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["cloudtrail.us-east-1.amazonaws.com",
                "cloudtrail.us-west-1.amazonaws.com",
                "cloudtrail.us-west-2.amazonaws.com",
                "cloudtrail.eu-west-1.amazonaws.com",
                "cloudtrail.eu-central-1.amazonaws.com",
                "cloudtrail.ap-northeast-1.amazonaws.com",
                "cloudtrail.ap-northeast-2.amazonaws.com",
                "cloudtrail.ap-southeast-1.amazonaws.com",
                "cloudtrail.ap-southeast-2.amazonaws.com",
                "cloudtrail.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ cloudtrail_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_dynamodb_test() ->
    Service = <<"dynamodb">>,
    ServiceAlt = <<"ddb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["dynamodb.us-east-1.amazonaws.com",
                "dynamodb.us-west-1.amazonaws.com",
                "dynamodb.us-west-2.amazonaws.com",
                "dynamodb.eu-west-1.amazonaws.com",
                "dynamodb.eu-central-1.amazonaws.com",
                "dynamodb.ap-northeast-1.amazonaws.com",
                "dynamodb.ap-northeast-2.amazonaws.com",
                "dynamodb.ap-southeast-1.amazonaws.com",
                "dynamodb.ap-southeast-2.amazonaws.com",
                "dynamodb.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_dynamodb_streams_test() ->
    Service = <<"streams.dynamodb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["streams.dynamodb.us-east-1.amazonaws.com",
                "streams.dynamodb.us-west-1.amazonaws.com",
                "streams.dynamodb.us-west-2.amazonaws.com",
                "streams.dynamodb.eu-west-1.amazonaws.com",
                "streams.dynamodb.eu-central-1.amazonaws.com",
                "streams.dynamodb.ap-northeast-1.amazonaws.com",
                "streams.dynamodb.ap-northeast-2.amazonaws.com",
                "streams.dynamodb.ap-southeast-1.amazonaws.com",
                "streams.dynamodb.ap-southeast-2.amazonaws.com",
                "streams.dynamodb.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ddb_streams_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_ec2_test() ->
    Service = <<"ec2">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["ec2.us-east-1.amazonaws.com",
                "ec2.us-west-1.amazonaws.com",
                "ec2.us-west-2.amazonaws.com",
                "ec2.eu-west-1.amazonaws.com",
                "ec2.eu-central-1.amazonaws.com",
                "ec2.ap-northeast-1.amazonaws.com",
                "ec2.ap-northeast-2.amazonaws.com",
                "ec2.ap-southeast-1.amazonaws.com",
                "ec2.ap-southeast-2.amazonaws.com",
                "ec2.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ec2_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_elasticloadbalancing_test() ->
    Service = <<"elasticloadbalancing">>,
    ServiceAlt = <<"elb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["elasticloadbalancing.us-east-1.amazonaws.com",
                "elasticloadbalancing.us-west-1.amazonaws.com",
                "elasticloadbalancing.us-west-2.amazonaws.com",
                "elasticloadbalancing.eu-west-1.amazonaws.com",
                "elasticloadbalancing.eu-central-1.amazonaws.com",
                "elasticloadbalancing.ap-northeast-1.amazonaws.com",
                "elasticloadbalancing.ap-northeast-2.amazonaws.com",
                "elasticloadbalancing.ap-southeast-1.amazonaws.com",
                "elasticloadbalancing.ap-southeast-2.amazonaws.com",
                "elasticloadbalancing.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ elb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ elb_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_iam_test() ->
    Service = <<"iam">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = lists:duplicate( length(Regions), "iam.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ iam_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_kinesis_test() ->
    Service = <<"kinesis">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["kinesis.us-east-1.amazonaws.com",
                "kinesis.us-west-1.amazonaws.com",
                "kinesis.us-west-2.amazonaws.com",
                "kinesis.eu-west-1.amazonaws.com",
                "kinesis.eu-central-1.amazonaws.com",
                "kinesis.ap-northeast-1.amazonaws.com",
                "kinesis.ap-northeast-2.amazonaws.com",
                "kinesis.ap-southeast-1.amazonaws.com",
                "kinesis.ap-southeast-2.amazonaws.com",
                "kinesis.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ kinesis_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_mechanicalturk_test() ->
    Service = <<"mechanicalturk">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = lists:duplicate( length(Regions),
                                "mechanicalturk.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ mturk_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_rds_test() ->
    Service = <<"rds">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["rds.us-east-1.amazonaws.com",
                "rds.us-west-1.amazonaws.com",
                "rds.us-west-2.amazonaws.com",
                "rds.eu-west-1.amazonaws.com",
                "rds.eu-central-1.amazonaws.com",
                "rds.ap-northeast-1.amazonaws.com",
                "rds.ap-northeast-2.amazonaws.com",
                "rds.ap-southeast-1.amazonaws.com",
                "rds.ap-southeast-2.amazonaws.com",
                "rds.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ rds_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_s3_test() ->
    Service = <<"s3">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"us-gov-west-1">>, <<"cn-north-1">>],
    Expected = ["s3-external-1.amazonaws.com",
                "s3-us-west-1.amazonaws.com",
                "s3-us-west-2.amazonaws.com",
                "s3-eu-west-1.amazonaws.com",
                "s3-eu-central-1.amazonaws.com",
                "s3-ap-northeast-1.amazonaws.com",
                "s3-ap-northeast-2.amazonaws.com",
                "s3-ap-southeast-1.amazonaws.com",
                "s3-ap-southeast-2.amazonaws.com",
                "s3-sa-east-1.amazonaws.com",
                "s3-fips-us-gov-west-1.amazonaws.com",
                "s3.cn-north-1.amazonaws.com.cn"],
    ?assertEqual( Expected,
                  [H || #aws_config{ s3_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sdb_test() ->
    Service = <<"sdb">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>],
    Expected = ["sdb.amazonaws.com",
                "sdb.us-west-1.amazonaws.com",
                "sdb.us-west-2.amazonaws.com",
                "sdb.eu-west-1.amazonaws.com",
                "sdb.eu-central-1.amazonaws.com",
                "sdb.ap-northeast-1.amazonaws.com",
                "sdb.ap-southeast-1.amazonaws.com",
                "sdb.ap-southeast-2.amazonaws.com",
                "sdb.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sdb_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_ses_test() ->
    Service = <<"ses">>,
    Regions = [<<"us-east-1">>, <<"us-west-2">>,
               <<"eu-west-1">>],
    Expected = ["email.us-east-1.amazonaws.com",
                "email.us-west-2.amazonaws.com",
                "email.eu-west-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ ses_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sns_test() ->
    Service = <<"sns">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"sa-east-1">>,
               <<"us-gov-west-1">>, <<"cn-north-1">>],
    Expected = ["sns.us-east-1.amazonaws.com",
                "sns.us-west-1.amazonaws.com",
                "sns.us-west-2.amazonaws.com",
                "sns.eu-west-1.amazonaws.com",
                "sns.eu-central-1.amazonaws.com",
                "sns.ap-northeast-1.amazonaws.com",
                "sns.ap-northeast-2.amazonaws.com",
                "sns.ap-southeast-1.amazonaws.com",
                "sns.ap-southeast-2.amazonaws.com",
                "sns.sa-east-1.amazonaws.com",
                "sns.us-gov-west-1.amazonaws.com",
                "sns.cn-north-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sns_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sqs_test() ->
    Service = <<"sqs">>,
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"us-gov-west-1">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"cn-north-1">>,
               <<"sa-east-1">>],
    Expected = ["sqs.us-east-1.amazonaws.com",
                "sqs.us-west-1.amazonaws.com",
                "sqs.us-west-2.amazonaws.com",
                "sqs.us-gov-west-1.amazonaws.com",
                "sqs.eu-west-1.amazonaws.com",
                "sqs.eu-central-1.amazonaws.com",
                "sqs.ap-northeast-1.amazonaws.com",
                "sqs.ap-northeast-2.amazonaws.com",
                "sqs.ap-southeast-1.amazonaws.com",
                "sqs.ap-southeast-2.amazonaws.com",
                "sqs.cn-north-1.amazonaws.com",
                "sqs.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sqs_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

service_config_sts_test() ->
    Service = <<"sts">>,
    ServiceAlt = sts,
    ServiceAlt2 = "sts",
    Regions = [<<"us-east-1">>, <<"us-west-1">>, <<"us-west-2">>,
               <<"us-gov-west-1">>,
               <<"eu-west-1">>, <<"eu-central-1">>,
               <<"ap-northeast-1">>, <<"ap-northeast-2">>,
               <<"ap-southeast-1">>, <<"ap-southeast-2">>,
               <<"cn-north-1">>,
               <<"sa-east-1">>],
    RegionsAlt = ["us-east-1", "us-west-1", "us-west-2",
                  "us-gov-west-1",
                  "eu-west-1", "eu-central-1",
                  "ap-northeast-1", "ap-northeast-2",
                  "ap-southeast-1", "ap-southeast-2",
                  "cn-north-1",
                  "sa-east-1"],
    Expected = ["sts.us-east-1.amazonaws.com",
                "sts.us-west-1.amazonaws.com",
                "sts.us-west-2.amazonaws.com",
                "sts.us-gov-west-1.amazonaws.com",
                "sts.eu-west-1.amazonaws.com",
                "sts.eu-central-1.amazonaws.com",
                "sts.ap-northeast-1.amazonaws.com",
                "sts.ap-northeast-2.amazonaws.com",
                "sts.ap-southeast-1.amazonaws.com",
                "sts.ap-southeast-2.amazonaws.com",
                "sts.cn-north-1.amazonaws.com",
                "sts.sa-east-1.amazonaws.com"],
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt, Region, #aws_config{} )
                             || Region <- RegionsAlt]] ),
    ?assertEqual( Expected,
                  [H || #aws_config{ sts_host = H } <-
                            [erlcloud_aws:service_config(
                               ServiceAlt2, Region, #aws_config{} )
                             || Region <- RegionsAlt]] ).

service_config_waf_test() ->
    Service = <<"waf">>,
    Regions = [<<"us-east-1">>],
    Expected = lists:duplicate( length(Regions), "waf.amazonaws.com" ),
    ?assertEqual( Expected,
                  [H || #aws_config{ waf_host = H } <-
                            [erlcloud_aws:service_config(
                               Service, Region, #aws_config{} )
                             || Region <- Regions]] ).

