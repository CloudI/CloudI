%% @doc Multicast Erlang node discovery protocol.
%% Listens on a multicast channel for node discovery requests and 
%% responds by connecting to the node.
%% @hidden
%% @end

-module (nodefindersrv).
-behaviour (gen_server).
-export ([ start_link/3, discover/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).

-record (state, { sendsock, recvsock, addr, port }).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Addr, Port, Ttl) ->
  gen_server:start_link ({ local, ?MODULE }, ?MODULE, [ Addr, Port, Ttl ], []).

discover () ->
  gen_server:call (?MODULE, discover).

%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

init ([ Addr, Port, Ttl ]) ->

  Opts = [ { active, true },
           { ip, Addr },
           { add_membership, { Addr, { 0, 0, 0, 0 } } },
           { multicast_loop, true },
           { reuseaddr, true },
           list ],

  { ok, RecvSocket } = gen_udp:open (Port, Opts),

  { ok, discover (#state{ recvsock = RecvSocket,
                          sendsock = send_socket (Ttl),
                          addr = Addr,
                          port = Port }) }.

handle_call (discover, _From, State) -> { reply, ok, discover (State) };
handle_call (_Request, _From, State) -> { noreply, State }.

handle_cast (_Request, State) -> { noreply, State }.

handle_info ({ udp, Socket, IP, InPortNo, Packet },
             State=#state{ recvsock = Socket }) ->
  { noreply, process_packet (Packet, IP, InPortNo, State) };

handle_info (_Msg, State) -> { noreply, State }.

terminate (_Reason, State = #state{}) ->
  gen_udp:close (State#state.recvsock),
  gen_udp:close (State#state.sendsock),
  ok.

code_change (_OldVsn, State, _Extra) -> 
  { ok, State }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

discover (State) ->
  NodeString = atom_to_list (node ()),
  Time = seconds (),
  Mac = mac ([ <<Time:64>>, NodeString ]),
  Message = [ "DISCOVERV2 ", Mac, " ", <<Time:64>>, " ", NodeString ],
  ok = gen_udp:send (State#state.sendsock,
                     State#state.addr,
                     State#state.port,
                     Message),
  State.

mac (Message) ->
  % Don't use cookie directly, creates a known-plaintext attack on cookie.
  % hehe ... as opposed to using ps :)
  Key = crypto:hash(sha, erlang:term_to_binary (erlang:get_cookie ())),
  crypto:hmac(sha, Key, Message).

process_packet ("DISCOVERV2 " ++ Rest, IP, InPortNo, State) -> 
  % Falling a mac is not really worth logging, since having multiple
  % cookies on the network is one way to prevent crosstalk.  However
  % the packet should always have the right structure.

  try
    <<Mac:20/binary, " ", 
      Time:64, " ",
      NodeString/binary>> = list_to_binary (Rest),

    case { mac ([ <<Time:64>>, NodeString ]), abs (seconds () - Time) } of
      { Mac, AbsDelta } when AbsDelta < 300 ->
        net_adm:ping (list_to_atom (binary_to_list (NodeString)));
      { Mac, AbsDelta } ->
        error_logger:warning_msg ("expired DISCOVERV2 (~p) from ~p:~p~n",
                                  [ AbsDelta,
                                    IP,
                                    InPortNo ]);
      _ ->
        ok
    end
  catch
    error : { badmatch, _ } ->
      error_logger:warning_msg ("bad DISCOVERV2 from ~p:~p~n", 
                                [ list_to_binary (Rest),
                                  IP,
                                  InPortNo ])
  end,

  State;
process_packet (_Packet, _IP, _InPortNo, State) -> 
  State.

seconds () ->
  calendar:datetime_to_gregorian_seconds (calendar:universal_time ()).

send_socket (Ttl) ->
  SendOpts = [ { ip, { 0, 0, 0, 0 } },
               { multicast_ttl, Ttl }, 
               { multicast_loop, true } ],

  { ok, SendSocket } = gen_udp:open (0, SendOpts),

  SendSocket.
