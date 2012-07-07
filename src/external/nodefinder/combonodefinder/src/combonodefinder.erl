%% @doc nodefinder+ec2nodefinder service.
%% @end

-module (combonodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/2, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> ok
%% @doc Initiate a discovery request.  Nodes will respond 
%% synchronously (on EC2) or asynchronously (not on EC2) 
%% and therefore should not necessarily be considered added to the 
%% erlang node list subsequent to this call returning.
%% @end

discover () ->
  case erlang:whereis(nodefindersrv) of
    undefined -> ec2nodefindersrv:discover ();
    _ -> nodefindersrv:discover ()
  end.

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start (Type, Args) ->
  case is_ec2_host () of
    true -> ec2nodefinder:start (Type, Args);
    false -> nodefinder:start (Type, Args)
  end.

%% @hidden

stop (State) ->
  case erlang:whereis(nodefindersrv) of
    undefined -> ec2nodefinder:stop (State);
    _ -> nodefinder:stop (State)
  end.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

is_ec2_host () ->
  {ok, HostNameShort} = inet:gethostname(),
  case inet_res:gethostbyname(HostNameShort) of
    {ok, {hostent, HostNameLong, _, _, _, _}} ->
      case lists:reverse (string:tokens (HostNameLong, ".")) of
        [ "internal", "ec2" | _ ] -> true;
        _ -> false
      end;
    _ -> false
  end.

