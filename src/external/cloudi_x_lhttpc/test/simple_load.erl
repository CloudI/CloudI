%%% -*- coding: latin-1 -*-
-module(simple_load).
-behaviour(gen_httpd).

-export([start_client/2, start_client/3]).
-export([client/2]).

-export([start_server/0]).
-export([init/2, handle_continue/5, handle_request/6, terminate/2]).

%%% Client part
start_client(Port, Clients) ->
	start_client("localhost", Port, Clients).

start_client(Host, Port, Clients) when Clients > 0 ->
	start_applications([crypto, ssl, lhttpc]),
	process_flag(trap_exit, true),
	{ok, Body} = file:read_file("test/1M"),
	URL = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/static/1M",
	start(Clients, URL, Body, Clients).

start(0, _, _, No) ->
	wait_exit(No, []);
start(Clients, URL, Body, No) ->
	spawn_link(?MODULE, client, [URL, Body]),
	start(Clients - 1, URL, Body, No).

wait_exit(0, []) ->
	ok;
wait_exit(0, Errors) ->
	{error, Errors};
wait_exit(No, Errors) ->
	receive
		{'EXIT', _, normal} ->
			wait_exit(No - 1, Errors);
		{'EXIT', _, Reason} ->
			wait_exit(No - 1, [Reason | Errors])
	end.

client(URL, Body) ->
	case lhttpc:request(URL, "POST", [], Body, 60000) of
		{ok, {{200, _}, _, Body}} ->
			ok;
		Other ->
			exit({bad_result, Other})
	end.

%%% Server part
start_server() ->
	SockOpts = [{backlog, 10000}],
	{ok, Pid} = gen_httpd:start_link(?MODULE, nil, 0, 600000, SockOpts),
	gen_httpd:port(Pid).

init(_, _) ->
	{ok, nil}.

handle_continue(_Method, _URI, _Vsn, _ReqHdrs, CBState) ->
	{continue, [], CBState}.

handle_request(_Method, "/static/1M", {1,1}, _, EntityBody, State) ->
	case EntityBody of
		{identity, EntityState} ->
			case gen_httpd:read_body(complete, 50000, EntityState) of
				{ok, {Body, http_eob}} ->
					{reply, 200, [], Body, State};
				{error, Reason} ->
					{reply, 500, [], io_lib:format("~p", [Reason]), State}
			end;
		_ ->
			{reply, 406, [], <<"No request body">>, State}
	end.

terminate(_, _) ->
	ok.

start_applications(Apps) ->
	Started = lists:map(fun({Name, _, _}) -> Name end,
		application:which_applications()),
	lists:foreach(fun(App) ->
				case lists:member(App, Started) of
					false -> ok = application:start(App);
					true  -> ok
				end
		end, Apps).
