Elli Websocket Handler
======================

elli_websocket is a websocket handler for Elli: https://github.com/knutin/elli

## Installation

You can add elli_websocket to your application by adding it as a dependency to your elli 
application. At the moment elli_websocket is dependant on a handover and ssl feature and
needs a specific elli branch.

```erlang
% rebar.config
{deps, [
    {elli, ".*", {git, "git://github.com/mmzeeman/elli.git", {branch, "ssl+handover"}}},
    {elli_websocket, ".*", {git, "git://github.com/mmzeeman/elli_websocket.git", {branch, "master"}}},
    % ...
]}.
```

Afterwards you can run:

```sh
$ ./rebar get-deps
$ ./rebar compile
```


## Callback module

See elli_example_websocket.erl for details.

```erlang
-module(elli_echo_websocket_handler).
-export([websocket_init/1, websocket_handle/3, websocket_info/3, websocket_handle_event/3]).

websocket_init(Req, Opts) ->
    State = undefined,
    {ok, [], State}.

websocket_handle(_Req, {text, Data}, State) ->
    {reply, {text, Data}, State};
websocket_handle(_Req, {binary, Data}, State) ->
    {reply, {binary, Data}, State};
websocket_handle(_Req, _Frame, State) ->
    {ok, State}.

websocket_info(Req, Message, State) ->
    {ok, State}.

websocket_handle_event(Name, EventArgs, State) ->
    ok.

```

## Upgrading to Websocket Connection

This is an example of how you can upgrade to a websocket connection.

```erlang

-module(elli_echo_websocket).

-export([init/2, handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

init(Req, Args) ->
    Method = case elli_request:get_header(<<"Upgrade">>, Req) of
        <<"websocket">> ->
            init_ws(elli_request:path(Req), Req, Args);
        _ ->
            ignore
    end.

handle(Req, Args) ->
    Method = case elli_request:get_header(<<"Upgrade">>, Req) of
        <<"websocket">> -> 
            websocket;
        _ ->
            elli_request:method(Req)        
    end,
    handle(Method, elli_request:path(Req), Req, Args).
    
handle_event(_Event, _Data, _Args) ->
    ok.

%%
%% Helpers
%%

init_ws([<<"echo_websocket">>], _Req, _Args) ->
    {ok, handover};
init_ws(_, _, _) ->
    ignore.

handle('websocket', [<<"echo_websocket">>], Req, Args) ->
    %% Upgrade to a websocket connection. 
    elli_websocket:upgrade(Req, Args),
    
    %% websocket is closed.
    {close, <<>>};

handle('GET', [<<"echo_websocket">>], _Req, _Args) ->
    %% We got a normal request, request was not upgraded.
    {200, [], <<"Use an upgrade request">>};

handle(_,_,_,_) ->
    ignore.
```




