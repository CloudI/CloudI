%% @doc S3 based node discovery service.
%% 
%% First, upon startup will enter a key of the form node.XXXXX into a specified
%% s3 bucket, where XXXXX is the node name.  Upon shutdown,
%% it will remove this key.
%%
%% Second, it will query a specified s3 bucket for keys of the form 
%% node.XXXXX where XXXXX is the node name.  It will try to 
%% connect to each one it finds.
%%
%% NB: This might fail when the number of nodes exceeds 1000, as there
%% is no attempt to page through the AWS list results.  

-module (s3nodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/2, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> ok
%% @doc Initiate a discovery request.  
%% @end

discover () ->
  s3nodefindersrv:discover ().

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start (_Type, _Args) ->
  { ok, AccessKeyIdFile } = application:get_env (s3nodefinder, 
                                                 access_key_id_file),
  { ok, AccessKeyIdBinary } = file:read_file (AccessKeyIdFile),
  { ok, SecretAccessKeyFile } = application:get_env (s3nodefinder, 
                                                     secret_access_key_file),
  { ok, Bucket } = application:get_env (s3nodefinder, bucket),
  s3nodefindersup:start_link (binary_to_list (AccessKeyIdBinary),
                              SecretAccessKeyFile, 
                              Bucket).

%% @hidden

stop (_State) ->
  ok.
