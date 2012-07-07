%% @doc ec2-describe-instances based nodefinder service.
%% @end

-module (ec2nodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/2, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> { ok, [ { Node::node (), pong | pang | timeout } ] }
%% @doc Initiate a discovery request.  Discovery is synchronous; the
%% results are returned.
%% @end

discover () ->
  ec2nodefindersrv:discover ().

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start (_Type, _Args) ->
  { ok, Group } = application:get_env (ec2nodefinder, group),
  { ok, PingTimeout } = application:get_env (ec2nodefinder, ping_timeout_sec),
  { ok, PrivateKey } = application:get_env (ec2nodefinder, private_key),
  { ok, Cert } = application:get_env (ec2nodefinder, cert),
  { ok, Ec2Home } = application:get_env (ec2nodefinder, ec2_home),
  { ok, JavaHome } = application:get_env (ec2nodefinder, java_home),

  ec2nodefindersup:start_link (Group, 
                               1000 * PingTimeout,
                               PrivateKey,
                               Cert,
                               Ec2Home,
                               JavaHome).

%% @hidden

stop (_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

