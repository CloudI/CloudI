%% Feel free to use, reuse and abuse the code in this file.

-module(http_init_shutdown).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, _Opts) ->
	{ok, Req2} = cowboy_req:reply(<<"666 Init Shutdown Testing">>,
		[{<<"connection">>, <<"close">>}], Req),
	{shutdown, Req2, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [], "Hello world!", Req),
	{ok, Req2, State}.

terminate(_, _, _) ->
	ok.
