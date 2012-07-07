%% @hidden
%% @doc ec2-describe-instances based node discovery service.
%% @end

-module (ec2nodefindersrv).
-behaviour (gen_server).
-export ([ start_link/6, discover/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).

-record (state, { group,
                  ping_timeout,
                  private_key,
                  cert,
                  ec2_home,
                  java_home }).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Group, PingTimeout, PrivateKey, Cert, Ec2Home, JavaHome)
  when is_list (Group),
       is_integer (PingTimeout),
       is_list (PrivateKey),
       is_list (Cert),
       is_list (Ec2Home),
       is_list (JavaHome) ->
  gen_server:start_link 
    ({ local, ?MODULE }, 
     ?MODULE, 
     [ Group, PingTimeout, PrivateKey, Cert, Ec2Home, JavaHome ], 
     []).

discover () ->
  gen_server:call (?MODULE, discover, 120000).

%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

init ([ Group, PingTimeout, PrivateKey, Cert, Ec2Home, JavaHome ]) ->
  pong = net_adm:ping (node ()), % don't startup unless distributed

  { ok, _ } = file:read_file_info (PrivateKey),
  { ok, _ } = file:read_file_info (Cert),

  State = #state{ group = Group,
                  ping_timeout = PingTimeout,
                  private_key = PrivateKey,
                  cert = Cert,
                  ec2_home = Ec2Home,
                  java_home = JavaHome },
  discover (State),
  { ok, State }.

handle_call (discover, _From, State) -> 
  { reply, { ok, discover (State) }, State };
handle_call (_Request, _From, State) -> 
  { noreply, State }.

handle_cast (_Request, State) -> { noreply, State }.

handle_info (_Msg, State) -> { noreply, State }.

terminate (_Reason, _State) -> ok.

code_change (_OldVsn, State, _Extra) -> { ok, State }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

async (Fun, Timeout) ->
  Me = self (),
  Ref = make_ref (),
  spawn (fun () ->
           { ok, _ } = timer:kill_after (Timeout),
           Me ! { Ref, Fun () }
         end),

  Ref.

collect (Key, Timeout) ->
  receive
    { Key, Status } -> Status
  after Timeout ->
    timeout
  end.

discover (State) ->
  Command = "env EC2_HOME='" ++ shell_escape (State#state.ec2_home) ++ "' " ++
            "JAVA_HOME='" ++ shell_escape (State#state.java_home) ++ "' " ++
            "ec2-describe-instances " ++
            "-K '" ++ shell_escape (State#state.private_key) ++ "' " ++
            "-C '" ++ shell_escape (State#state.cert) ++ "' " ++
            "2>/dev/null",

  Output = os:cmd (Command),

  Group = State#state.group,
  Timeout = State#state.ping_timeout,

  [ { Node, collect (Key2, Timeout) } ||
    { Node, Key2 } <- 
      [ { Node, start_ping (Node, Timeout) } ||
        { Host, { ok, NamesAndPorts } } <- 
          [ { Host, collect (Key, Timeout) } ||
            { Host, Key } <- [ { Host, start_names (Host, Timeout) } 
                            || Host <- parse_host_list (Output, Group) ] ],
        { Name, _ } <- NamesAndPorts,
      Node <- [ list_to_atom (Name ++ "@" ++ Host) ] ] ].

%RESERVATION	r-817d94e8	038239462139	default
%INSTANCE	i-72f4021b	ami-81d93ce8	ec2-67-202-6-8.z-1.compute-1.amazonaws.com	domU-12-31-36-00-0C-02.z-1.compute-1.internal	shutting-down	paul	1		m1.small	2007-12-10T22:26:49+0000
%RESERVATION	r-f0927a99	038239462139	default
%INSTANCE	i-290dfb40	ami-81d93ce8	ec2-67-202-18-240.compute-1.amazonaws.com	domU-12-31-38-00-38-E6.compute-1.internal	running	paul	0		m1.small	2007-12-15T00:35:20+0000
%RESERVATION	r-d9927ab0	038239462139	flass
%INSTANCE	i-310dfb58	ami-81d93ce8	ec2-72-44-51-185.z-1.compute-1.amazonaws.com	domU-12-31-36-00-31-03.z-1.compute-1.internal	running	paul	0		m1.small	2007-12-15T00:37:05+0000

parse_host_list (Output, Group) ->
  parse_host_list 
    ([ string:tokens (Line, "\t") || Line <- string:tokens (Output, "\n") ],
     Group,
     false,
     []).

parse_host_list ([], _, _, Acc) -> 
  Acc;
parse_host_list ([ [] | Rest ], Group, State, Acc) ->
  parse_host_list (Rest, Group, State, Acc);
parse_host_list ([ Line | Rest ], Group, _, Acc) 
  when hd (Line) =:= "RESERVATION",
       length (Line) >= 4 ->
  parse_host_list (Rest, Group, hd (tl (tl (tl (Line)))) =:= Group, Acc);
parse_host_list ([ Line | Rest ], Group, true, Acc) 
  when hd (Line) =:= "INSTANCE",
       length (Line) >= 4 ->
  parse_host_list (Rest, Group, true, [ hd (tl (tl (tl (Line)))) | Acc ]);
parse_host_list ([ _ | Rest ], Group, State, Acc) ->
  parse_host_list (Rest, Group, State, Acc).

shell_escape (String) -> shell_escape (String, []).

shell_escape ([], Acc) -> lists:reverse (Acc);
shell_escape ([ $' | T ], Acc) -> shell_escape (T, [ $', $\\ | Acc ]);
shell_escape ([ $\\ | T ], Acc) -> shell_escape (T, [ $\\, $\\ | Acc ]);
shell_escape ([ H | T ], Acc) -> shell_escape (T, [ H | Acc ]).

start_names (Host, Timeout) ->
  async (fun () -> net_adm:names (Host) end, Timeout).

start_ping (Node, Timeout) ->
  async (fun () -> net_adm:ping (Node) end, Timeout).
