

-module(ws_test).

-export([start/0]).


start() -> 
    _ = application:start(crypto),
    _ = application:start(public_key),
    _ = application:start(ssl),
    
    WsConfig = [{handler, elli_example_websocket}],

    Config = [{mods, [{elli_example_websocket, WsConfig}]}],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}, {port, 8000}]).
