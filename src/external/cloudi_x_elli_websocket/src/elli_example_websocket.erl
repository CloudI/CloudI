%%
%%
%%


-module(elli_example_websocket).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([init/2, handle/2, handle_event/3]).

% Websocket callbacks.
-export([
    websocket_init/2, 
    websocket_info/3, 
    websocket_handle/3,

    websocket_handle_event/3
]).

-include_lib("elli/include/elli.hrl").

%% Serves as elli and websocket handler in one module. 

-behaviour(elli_handler).
-behaviour(elli_websocket_handler).

%%
%% Elli Handler Callbacks
%%

%% It would be nice if it was possbile to somehow pass the fact
%% that this request is upgraded.
%%
init(Req, Args) ->
    _Method = case elli_request:get_header(<<"Upgrade">>, Req) of
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


%%
%% Elli handler callbacks
%%

init_ws([<<"my">>, <<"websocket">>], _Req, _Args) ->
    {ok, handover};
init_ws(_, _, _) ->
    ignore.

handle('websocket', [<<"my">>, <<"websocket">>], Req, Args) ->
    elli_websocket:upgrade(Req, Args),
    {close, <<>>};

handle('GET', [<<"my">>, <<"websocket">>], _Req, _Args) ->
    {200, [], <<"Use an upgrade request">>};

handle(_,_,_,_) ->
    ignore.

handle_event(Name, EventArgs, ElliArgs) ->
    io:fwrite(standard_error, "event: ~p ~p ~p~n", [Name, EventArgs, ElliArgs]),
    ok.

%%
%% Elli websocket handler callbacks
%%


% @doc
%
websocket_init(Req, Opts) ->
    io:fwrite(standard_error, "example_ws_init: ~p, ~p ~n", [Req, Opts]),
    State = undefined,
    {ok, [], State}.

websocket_info(_Req, Message, State) ->
    io:fwrite(standard_error, "example_ws_info: ~p~n", [Message]),
    {ok, State}.

websocket_handle(_Req, Message, State) ->
    io:fwrite(standard_error, "example_ws_handle: ~p~n", [Message]),
%%  default behaviour.
    {ok, State}.
%%  comment the line above ({ok, State}.)
%%  and uncomment the line below for an echo server.
%   {reply, Message, State}.


%%
%% Elli Websocket Event Callbacks.
%%

%% websocket_open and websocket_close events are sent when the websocket
%% opens, and when it closes.
websocket_handle_event(websocket_open, [_, _Version, _Compress], _) -> ok;
websocket_handle_event(websocket_close, [_, _Reason], _) -> ok;

%% websocket_throw, websocket_error and websocket_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
websocket_handle_event(websocket_throw, [_Request, _Exception, _Stacktrace], _) -> ok;
websocket_handle_event(websocket_error, [_Request, _Exception, _Stacktrace], _) -> ok;
websocket_handle_event(websocket_exit, [_Request, _Exception, _Stacktrace], _) -> ok.



    
