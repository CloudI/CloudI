%% @hidden

-module (s3nodefindersup).
-behaviour (supervisor).

-export ([ start_link/3, init/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (AccessKeyId, SecretAccessKeyFile, Bucket) ->
  supervisor:start_link (?MODULE, [ AccessKeyId, SecretAccessKeyFile, Bucket ]).

init ([ AccessKeyId, SecretAccessKeyFile, Bucket ]) ->
  { ok,
    { { one_for_one, 3, 10 },
      [ { s3nodefindersrv,
          { s3nodefindersrv, 
            start_link,
            [ AccessKeyId, SecretAccessKeyFile, Bucket ] },
          permanent,
          10000,
          worker,
          [ s3nodefindersrv ]
        }
      ]
    }
  }.
