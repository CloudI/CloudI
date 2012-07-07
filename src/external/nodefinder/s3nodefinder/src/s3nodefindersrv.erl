%% @hidden

-module (s3nodefindersrv).
-behaviour (gen_server).
-include_lib ("xmerl/include/xmerl.hrl").
-export ([ start_link/3, discover/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).

-record (state, { access_key_id, secret_access_key_file, bucket }).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (AccessKeyId, SecretAccessKeyFile, Bucket) ->
  gen_server:start_link ({ local, ?MODULE }, 
                         ?MODULE, 
                         [ AccessKeyId, SecretAccessKeyFile, Bucket ], 
                         [ { timeout, 30000 } ]).

discover () ->
  gen_server:call (?MODULE, discover, 30000).

%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

init ([ AccessKeyId, SecretAccessKeyFile, Bucket ]) ->
  pong = net_adm:ping (node ()), % don't startup unless distributed

  State = #state{ access_key_id = AccessKeyId,
                  secret_access_key_file = shell_escape (SecretAccessKeyFile),
                  bucket = ensure_bucket_name (Bucket) },
  { ok, discover (announce (State)) }.

handle_call (discover, _From, State) -> { reply, ok, discover (State) };
handle_call (_Request, _From, State) -> { noreply, State }.

handle_cast (_Request, State) -> { noreply, State }.

handle_info (_Msg, State) -> { noreply, State }.

terminate (_Reason, State) ->
  unannounce (State),
  ok.

code_change (_OldVsn, State, _Extra) -> { ok, State }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

announce (State) ->
  Command = "s3-put " ++
            "-k '" ++ State#state.access_key_id ++ "' " ++
            "-s '" ++ State#state.secret_access_key_file ++ "' " ++
            "-S " ++
            "-T /dev/null " ++ 
            "-H '/tmp/announce" ++ 
              shell_escape (atom_to_list (node ())) ++ "' " ++
            "'" ++ State#state.bucket ++ 
                   "node." ++ shell_escape (atom_to_list (node ())) ++ "'",

  os:cmd (Command),
  State.

collect_ping (Key, Timeout) ->
  receive
    { Key, Status } -> Status
  after Timeout ->
    pang
  end.

delete (Node, State) when is_atom (Node) -> delete (atom_to_list (Node), State);
delete (Node, State) ->
  Command = "s3-delete " ++
            "-k '" ++ State#state.access_key_id ++ "' " ++
            "-s '" ++ State#state.secret_access_key_file ++ "' " ++
            "-S " ++
            "-H '/tmp/delete" ++ 
              shell_escape (atom_to_list (node ())) ++ "' " ++
            "'" ++ State#state.bucket ++ "node." ++ shell_escape (Node) ++ "'",

  os:cmd (Command),
  State.

discover (State) ->
  Command = "s3-get " ++
            "-k '" ++ State#state.access_key_id ++ "' " ++
            "-s '" ++ State#state.secret_access_key_file ++ "' " ++
            "-S " ++
            "-H '/tmp/discover" ++ 
              shell_escape (atom_to_list (node ())) ++ "' " ++
            "'" ++ State#state.bucket ++ "?prefix=node' " ++ 
            "2>/dev/null",

  Output = os:cmd (Command),

  Timeout = 3000,

  BadNodes = [ Node || { Node, Key } <- [ { X, start_ping (X, Timeout) } || 
                                           X <- parse_node_list (Output) ],
                       collect_ping (Key, Timeout) =:= pang ],

  lists:foldl (fun (Node, S) -> delete (Node, S) end, State, BadNodes).

ensure_bucket_name (Bucket) -> 
  shell_escape (ensure_postfix (ensure_prefix (Bucket))).

ensure_prefix (Bucket = [ $/ | _ ]) -> Bucket;
ensure_prefix (Bucket) -> "/" ++ Bucket.

ensure_postfix (Bucket) -> 
  lists:reverse (ensure_prefix (lists:reverse (Bucket))).

parse_node_list (Output) ->
  { #xmlElement{ name = 'ListBucketResult', 
                 content = Content }, _ } = xmerl_scan:string (Output),
  [ list_to_atom (NodeName) 
    || #xmlElement{ name = 'Contents', 
                    content = SubContent } <- Content,
       #xmlElement{ name = 'Key', 
                    content = KeyContent } <- SubContent,
       #xmlText{ value = "node." ++ NodeName } <- KeyContent ].

shell_escape (String) -> shell_escape (String, []).

shell_escape ([], Acc) -> lists:reverse (Acc);
shell_escape ([ $' | T ], Acc) -> shell_escape (T, [ $', $\\ | Acc ]);
shell_escape ([ $\\ | T ], Acc) -> shell_escape (T, [ $\\, $\\ | Acc ]);
shell_escape ([ H | T ], Acc) -> shell_escape (T, [ H | Acc ]).

start_ping (Node, Timeout) ->
  Me = self (),
  Ref = make_ref (),
  spawn (fun () ->
           { ok, _ } = timer:kill_after (Timeout),
           Me ! { Ref, net_adm:ping (Node) }
         end),

  Ref.

unannounce (State) -> delete (node (), State).
