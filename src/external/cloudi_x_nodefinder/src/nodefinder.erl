%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(nodefinder).

%% external interface
-export([ec2_start/5,
         ec2_discover/1,
         ec2_stop/0,
         multicast_start/0,
         multicast_start/4,
         multicast_discover/1,
         multicast_stop/0,
         timeout_min/0]).

-spec ec2_start(AccessKeyID :: string(),
                SecretAccessKey :: string(),
                EC2Host :: string(),
                Groups :: list(nodefinder_ec2:group()),
                Tags :: list(nodefinder_ec2:tag())) ->
    {ok, pid()} |
    {error, any()}.

ec2_start(AccessKeyID, SecretAccessKey, EC2Host, Groups, Tags)
    when is_integer(hd(AccessKeyID)), is_integer(hd(SecretAccessKey)),
         is_integer(hd(EC2Host)), is_list(Groups), is_list(Tags) ->
    nodefinder_sup:start_child(nodefinder_ec2,
                               [AccessKeyID, SecretAccessKey, EC2Host,
                                Groups, Tags]).

-spec ec2_discover(Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

ec2_discover(Timeout)
    when is_integer(Timeout), Timeout > 0 ->
    nodefinder_ec2:discover(Timeout).

-spec ec2_stop() ->
    ok |
    {error, any()}.

ec2_stop() ->
    nodefinder_sup:stop_child(nodefinder_ec2).

-spec multicast_start() ->
    {ok, pid()} |
    {error, any()}.

multicast_start() ->
    multicast_start({224,0,0,1}, 4475, 1, 300).

-spec multicast_start(Addr :: inet:ip_address(),
                      Port :: inet:port_number(),
                      TTL :: non_neg_integer(),
                      TimeoutSeconds :: pos_integer()) ->
    {ok, pid()} |
    {error, any()}.

multicast_start(Addr, Port, TTL, TimeoutSeconds)
    when is_integer(Port), Port > 0,
         is_integer(TTL), TTL >= 0,
         is_integer(TimeoutSeconds), TimeoutSeconds > 0 ->
    nodefinder_sup:start_child(nodefinder_multicast,
                               [Addr, Port, TTL, TimeoutSeconds]).

-spec multicast_discover(Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

multicast_discover(Timeout)
    when is_integer(Timeout), Timeout > 0 ->
    nodefinder_multicast:discover(Timeout).

-spec multicast_stop() ->
    ok |
    {error, any()}.

multicast_stop() ->
    nodefinder_sup:stop_child(nodefinder_multicast).

-spec timeout_min() ->
    pos_integer().

timeout_min() ->
    net_kernel:connecttime() + 100.

