%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Michael Truog <mjtruog [at] gmail (dot) com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol
%% @doc a binary protocol memcached client
-module(ememcached).
-behaviour(gen_server).

%% external interface
-export([start_link/1, start_link/2]).
-export([find_next_largest/2]).

%% api interface
-export([get/2, get/3,
         get_many/2, get_many/3,
         add/3, add/4,
         add_exp/4, add_exp/5,
         set/3, set/4,
         set_exp/4, set_exp/5,
		 replace/3, replace/4,
         replace_exp/4, replace_exp/5,
         delete/2, delete/3,
         increment_exp/5, increment_exp/6,
         decrement_exp/5, decrement_exp/6,
		 append/3, append/4,
         prepend/3, prepend/4,
         stats/1, stats/2,
         flush/1, flush/2,
         flush_exp/2, flush_exp/3,
         quit/1, quit/2, 
		 version/1, version/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-include("ememcached.hrl").

-record(state, {continuum}).

-define(TIMEOUT, 5000).

%% @spec start_link(CacheServers, Timeout) -> {ok, pid()}
%%       CacheServers = [{Host, Port, ConnectionPoolSize}]
%%       Timeout = integer() | infinity
%%       Host = string()
%%       Port = integer()
%%       ConnectionPoolSize = integer()
%%
%% Timeout is used for the tcp connect
%%     (and all tcp sends if it is not infinity)
start_link(CacheServers) ->
    start_link(CacheServers, infinity).

start_link(CacheServers, Timeout)
    when is_list(CacheServers) ->
    gen_server:start_link(?MODULE, [CacheServers, Timeout], []).
    
get(Process, Key) ->
    get(Process, Key, ?TIMEOUT).

get(Process, Key, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {get, Key, Timeout}, Timeout + 5000).

get_many(Process, Keys) ->
    get_many(Process, Keys, ?TIMEOUT).
    
get_many(Process, Keys, Timeout)
    when is_list(Keys), is_integer(Timeout) ->
    gen_server:call(Process,
        {get_many, Keys, Timeout}, Timeout + 5000).
    
add(Process, Key, Value) ->
	add_exp(Process, Key, Value, 0, ?TIMEOUT).
	
add(Process, Key, Value, Timeout) ->
	add_exp(Process, Key, Value, 0, Timeout).
	
add_exp(Process, Key, Value, Expiration) ->
	add_exp(Process, Key, Value, Expiration, ?TIMEOUT).

add_exp(Process, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    gen_server:call(Process,
        {add, Key, Value, Expiration, Timeout}, Timeout + 5000).

set(Process, Key, Value) ->
	set_exp(Process, Key, Value, 0, ?TIMEOUT).
	
set(Process, Key, Value, Timeout) ->
	set_exp(Process, Key, Value, 0, Timeout).
	
set_exp(Process, Key, Value, Expiration) ->
    set_exp(Process, Key, Value, Expiration, ?TIMEOUT).
    
set_exp(Process, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    gen_server:call(Process,
        {set, Key, Value, Expiration, Timeout}, Timeout + 5000).
    
replace(Process, Key, Value) ->
	replace_exp(Process, Key, Value, 0, ?TIMEOUT).
	
replace(Process, Key, Value, Timeout) ->
	replace_exp(Process, Key, Value, 0, Timeout).
	
replace_exp(Process, Key, Value, Expiration) ->
    replace_exp(Process, Key, Value, Expiration, ?TIMEOUT).
    
replace_exp(Process, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    gen_server:call(Process,
        {replace, Key, Value, Expiration, Timeout}, Timeout + 5000).
    
delete(Process, Key) ->
    delete(Process, Key, ?TIMEOUT).

delete(Process, Key, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {delete, Key, Timeout}, Timeout + 5000).

increment_exp(Process, Key, Value, Initial, Expiration) ->
    increment_exp(Process, Key, Value, Initial, Expiration, ?TIMEOUT).

increment_exp(Process, Key, Value, Initial, Expiration, Timeout)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration),
         is_integer(Timeout) ->
    gen_server:call(Process,
        {increment, Key, Value, Initial, Expiration, Timeout}, Timeout + 5000).

decrement_exp(Process, Key, Value, Initial, Expiration) ->
    decrement_exp(Process, Key, Value, Initial, Expiration, ?TIMEOUT).

decrement_exp(Process, Key, Value, Initial, Expiration, Timeout)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration),
         is_integer(Timeout) ->
    gen_server:call(Process,
        {decrement, Key, Value, Initial, Expiration, Timeout}, Timeout + 5000).

append(Process, Key, Value) ->
    append(Process, Key, Value, ?TIMEOUT).

append(Process, Key, Value, Timeout)
    when is_binary(Value), is_integer(Timeout) ->
    gen_server:call(Process,
        {append, Key, Value, Timeout}, Timeout + 5000).

prepend(Process, Key, Value) ->
    prepend(Process, Key, Value, ?TIMEOUT).

prepend(Process, Key, Value, Timeout)
    when is_binary(Value), is_integer(Timeout) ->
    gen_server:call(Process,
        {prepend, Key, Value, Timeout}, Timeout + 5000).

stats(Process) ->
    stats(Process, ?TIMEOUT).

stats(Process, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {stats, Timeout}, Timeout + 5000).

flush(Process) ->
    flush(Process, ?TIMEOUT).
    
flush(Process, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {flush, Timeout}, Timeout + 5000).
    
flush_exp(Process, Expiration) ->
    flush_exp(Process, Expiration, ?TIMEOUT).
    
flush_exp(Process, Expiration, Timeout)
    when is_integer(Expiration), is_integer(Timeout) ->
    gen_server:call(Process,
        {flush, Expiration, Timeout}, Timeout + 5000).
    
quit(Process) ->
    quit(Process, ?TIMEOUT).
    
quit(Process, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {quit, Timeout}, Timeout + 5000).
    
version(Process) ->
    version(Process, ?TIMEOUT).

version(Process, Timeout)
    when is_integer(Timeout) ->
    gen_server:call(Process,
        {version, Timeout}, Timeout + 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         ignore      |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init([CacheServers, Timeout]) ->
    random:seed(erlang:now()),

    ConnectionOptions = [binary, {packet, 0}, {active, false}],
    ExtraConnectionOptions = if
        Timeout /= infinity ->
            [{send_timeout, Timeout}];
        true ->
            []
    end,

    % Continuum = [{uint(), {Host, Port, [socket()]}}]
    Continuum = lists:foldl(fun({Host, Port, PoolSize}, Acc0) ->
        Sockets = lists:foldl(fun(_, Acc1) ->
            case gen_tcp:connect(Host, Port, ConnectionOptions ++
                                 ExtraConnectionOptions, Timeout) of
                {ok, S} ->
                    [S | Acc1];
                _ ->
                    Acc1
            end
        end, [], lists:seq(1, PoolSize)),
        if
            erlang:length(Sockets) == PoolSize ->
                [{hash_to_uint(Host, Port),
                  {Host, Port, PoolSize, Sockets}} | Acc0];
            true ->
                Acc0
        end
    end, [], CacheServers),
    if
        erlang:length(Continuum) /= erlang:length(CacheServers) ->
            {stop, "error creating sockets"};
        true ->
            {ok, #state{continuum = lists:keysort(1, Continuum)}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------    
handle_call({get, Key0, Timeout}, _, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    #response{key=Key1, value=Value} =
        send_recv(Socket,
            #request{
                op_code=?OP_GetK,
                key=list_to_binary(Key)}, Timeout),
    case binary_to_list(Key1) of
        Key -> {reply, Value, State};
        _ -> {reply, <<>>, State}
    end;

handle_call({get_many, Keys, Timeout}, _, State) ->
    SocketDicts = lists:foldl(
        fun(Key, Dict) ->
            Socket = map_key(State, Key),
            send(Socket,
                 #request{
                     op_code=?OP_GetKQ,
                     key=list_to_binary(Key)}),
            case dict:find(Socket, Dict) of
                {ok, Count} -> dict:store(Socket, Count+1, Dict);
                error -> dict:store(Socket, 1, Dict)
            end
        end, dict:new(), Keys),
    Resps = lists:flatten([begin
        send(Socket, #request{op_code=?OP_Noop}),
        [recv(Socket, Timeout) || _ <- lists:seq(1,Count)]
     end || {Socket, Count} <- dict:to_list(SocketDicts)]),
    Reply = [begin
        case lists:keysearch(list_to_binary(Key), 8, Resps) of
            {value, Resp} -> {Key, Resp#response.value};
            false -> {Key, <<>>}
        end
     end || Key <- Keys],
    {reply, Reply, State};
    
handle_call({add, Key0, Value, Expiration, Timeout}, _, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket,
        #request{
            op_code=?OP_Add,
            extras = <<16#deadbeef:32, Expiration:32>>,
            key=list_to_binary(Key), value=Value}, Timeout),
    {reply, Resp#response.value, State};
    
handle_call({set, Key0, Value, Expiration, Timeout}, _, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket,
        #request{
            op_code=?OP_Set,
            extras = <<16#deadbeef:32, Expiration:32>>,
            key=list_to_binary(Key), value=Value}, Timeout),
    {reply, Resp#response.value, State};

handle_call({replace, Key0, Value, Expiration, Timeout}, _, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket,
        #request{
            op_code=?OP_Replace,
            extras = <<16#deadbeef:32, Expiration:32>>,
            key=list_to_binary(Key), value=Value}, Timeout),
    {reply, Resp#response.value, State};

handle_call({delete, Key0, Timeout}, _, State) ->
    Key = package_key(Key0),
    Socket = map_key(State, Key),
    Resp = send_recv(Socket,
        #request{
            op_code=?OP_Delete,
            key=list_to_binary(Key)}, Timeout),
    {reply, Resp#response.value, State};

handle_call({increment, Key0, Value, Initial, Expiration, Timeout}, _, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket,
        #request{
            op_code=?OP_Increment,
            extras = <<Value:64, Initial:64, Expiration:32>>,
            key=list_to_binary(Key)}, Timeout),
	{reply, Resp, State};
	
handle_call({decrement, Key0, Value, Initial, Expiration, Timeout}, _, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket,
        #request{
            op_code=?OP_Decrement,
            extras = <<Value:64, Initial:64, Expiration:32>>,
            key=list_to_binary(Key)}, Timeout),
	{reply, Resp, State};

handle_call({append, Key0, Value, Timeout}, _, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket,
        #request{
            op_code=?OP_Append,
            key=list_to_binary(Key),
            value=Value}, Timeout),
	{reply, Resp#response.value, State};

handle_call({prepend, Key0, Value, Timeout}, _, State) ->
	Key = package_key(Key0),
    Socket = map_key(State, Key),
	Resp = send_recv(Socket,
        #request{
            op_code=?OP_Prepend,
            key=list_to_binary(Key),
            value=Value}, Timeout),
	{reply, Resp#response.value, State};
	
handle_call({stats, Timeout}, _, State) ->
    Sockets = [begin
        {{Host, Port}, begin
            send(Socket, #request{op_code=?OP_Stat}),
            Socket
        end}
     end || {_, {Host, Port, _, [Socket|_]}} <- State#state.continuum],
    Reply = [begin
        {HostPortKey, collect_stats_from_socket(Socket, Timeout)}
    end || {HostPortKey, Socket} <- Sockets],
    {reply, Reply, State};

handle_call({flush, Timeout}, _, State) ->
    Reply = send_all(State, #request{op_code=?OP_Flush}, Timeout),
    {reply, Reply, State};
        
handle_call({flush, Expiration, Timeout}, _, State) ->
    Reply = send_all(State,
        #request{
            op_code=?OP_Flush,
            extras = <<Expiration:32>>}, Timeout),
    {reply, Reply, State};
    
handle_call({quit, Timeout}, _, State) ->
    Reply = send_all(State, #request{op_code=?OP_Quit}, Timeout),
    {reply, Reply, State};
    
handle_call({version, Timeout}, _, State) ->
    Reply = send_all(State, #request{op_code=?OP_Version}, Timeout),
    {reply, Reply, State};
			
handle_call(_, _, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Message, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_all(#state{continuum = Continuum}, Request, Timeout) ->
    [begin
        {{Host, Port}, begin
            Resp = send_recv(Socket, Request, Timeout),
            Resp#response.value
        end}
     end || {_, {Host, Port, _, [Socket|_]}} <- Continuum].
     
collect_stats_from_socket(Socket, Timeout) ->
    collect_stats_from_socket(Socket, [], Timeout).
    
collect_stats_from_socket(Socket, Acc, Timeout) ->
    case recv(Socket, Timeout) of
        #response{body_size=0} ->
            Acc;
        #response{key=Key, value=Value} ->
            collect_stats_from_socket(Socket,
                [{binary_to_atom(Key, utf8), binary_to_list(Value)} | Acc],
                Timeout)
    end.

send_recv(Socket, Request, Timeout) ->
    send(Socket, Request),
    recv(Socket, Timeout).
    
send(Socket, Request) ->
    Bin = encode_request(Request),
    case gen_tcp:send(Socket, Bin) of
        ok ->
            ok;
        {error, _} = Error ->
            erlang:exit(Error)
    end.

recv(Socket, Timeout) ->
    Resp1 = recv_header(Socket, Timeout),
    Resp2 = recv_body(Socket, Resp1, Timeout),
    Resp2.
        
encode_request(Request) when is_record(Request, request) ->
    Magic = 16#80,
    Opcode = Request#request.op_code,
    KeySize = size(Request#request.key),
    Extras = Request#request.extras,
    ExtrasSize = size(Extras),
    DataType = Request#request.data_type,
    Reserved = Request#request.reserved,
    Body = <<Extras:ExtrasSize/binary,
             (Request#request.key)/binary,
             (Request#request.value)/binary>>,
    BodySize = size(Body),
    Opaque = Request#request.opaque,
    CAS = Request#request.cas,
    <<Magic:8, Opcode:8, KeySize:16, ExtrasSize:8,
      DataType:8, Reserved:16, BodySize:32, Opaque:32,
      CAS:64, Body:BodySize/binary>>.

recv_header(Socket, Timeout) ->
    decode_response_header(
        recv_bytes(Socket, 24, Timeout)).
  
recv_body(Socket,
          #response{
              key_size = KeySize,
              extras_size = ExtrasSize,
              body_size = BodySize} = Resp, Timeout) ->
    decode_response_body(
        recv_bytes(Socket, BodySize, Timeout), ExtrasSize, KeySize, Resp).
    
decode_response_header(<<16#81:8, Opcode:8, KeySize:16, ExtrasSize:8,
                         DataType:8, Status:16, BodySize:32,
                         Opaque:32, CAS:64>>) ->
    #response{
        op_code = Opcode, 
        data_type = DataType, 
        status = Status, 
        opaque = Opaque, 
        cas = CAS, 
        key_size = KeySize,
        extras_size = ExtrasSize,
        body_size = BodySize
    }.
    
decode_response_body(Bin, ExtrasSize, KeySize, Resp) ->
    <<Extras:ExtrasSize/binary, Key:KeySize/binary, Value/binary>> = Bin,
    Resp#response{
        extras = Extras,
        key = Key,
        value = Value
    }.

recv_bytes(_, 0, _) ->
    <<>>;
recv_bytes(Socket, NumBytes, Timeout) ->
    case gen_tcp:recv(Socket, NumBytes, Timeout) of
        {ok, Bin} ->
            Bin;
        {error, _} = Error ->
            erlang:exit(Error)
    end.

package_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
    
package_key(Key) when is_list(Key) ->
    Key;
    
package_key(Key) when is_binary(Key) ->
    binary_to_list(Key);
    
package_key(Key) ->
    lists:flatten(io_lib:format("~p", [Key])).

%% Consistent hashing functions
%%
%% First, hash memcached servers to unsigned integers on a continuum. To
%% map a key to a memcached server, hash the key to an unsigned integer
%% and locate the next largest integer on the continuum. That integer
%% represents the hashed server that the key maps to.
%% reference: http://www8.org/w8-papers/2a-webserver/caching/paper2.html
hash_to_uint(Host, Port) when is_list(Host), is_integer(Port) ->
    hash_to_uint(Host ++ integer_to_list(Port)).

hash_to_uint(Key) when is_list(Key) -> 
    <<Hash1:32/unsigned-integer,
      Hash2:32/unsigned-integer,
      Hash3:32/unsigned-integer,
      Hash4:32/unsigned-integer>> = erlang:md5(Key),
    Hash1 bxor Hash2 bxor Hash3 bxor Hash4.

map_key(#state{continuum = Continuum}, Key) when is_list(Key) ->
    case find_next_largest(hash_to_uint(Key), Continuum) of
        {_, {_, _, 1, [Socket]}} ->
            Socket;
        {Hash, {_, _, PoolSize, [Socket | SocketList]}} ->
            Index = erlang:get(Hash),
            if
                Index == undefined ->
                    erlang:put(Hash, 2),
                    Socket;
                Index > PoolSize ->
                    erlang:put(Hash, 2),
                    Socket;
                true ->
                    erlang:put(Hash, Index + 1),
                    lists:nth(Index, SocketList)
            end
    end.

%% minimize list operations when searching the Continuum
find_next_largest(Int, [{Hash, Value} = Entry | Continuum])
    when is_integer(Int) ->
    if
        Hash > Int ->
            Entry;
        true ->
            % keep the first value in case no hash is larger than Int
            find_next_largest(Int, Continuum, Value)
    end.
find_next_largest(_, [], Value) ->
    {0, Value};
find_next_largest(Int, [{Hash, _} | Continuum], Value) when Hash =< Int ->
    find_next_largest(Int, Continuum, Value);
find_next_largest(_, [{_, _} = Entry | _], _) ->
    Entry.

