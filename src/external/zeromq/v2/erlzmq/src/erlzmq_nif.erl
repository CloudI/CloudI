%% @hidden
-module(erlzmq_nif).

-export([context/1,
         socket/4,
         bind/2,
         connect/2,
         send/3,
         recv/2,
         setsockopt/3,
         getsockopt/2,
         close/1,
         term/1,
         version/0]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir(erlzmq) of
        Path when is_list(Path) ->
            erlang:load_nif(filename:join([Path, "erlzmq_drv"]), []);
        {error, bad_name} ->
            case code:which(erlzmq_nif) of
                Filename when is_list(Filename) ->
                    erlang:load_nif(filename:join([filename:dirname(Filename),
                                                   "..","priv",
                                                   "erlzmq_drv"]), []);
                Reason when is_atom(Reason) ->
                    {error, Reason}
            end
    end.

context(_Threads) ->
    erlang:nif_error(not_loaded).

socket(_Context, _Type, _Active, _ActivePid) ->
    erlang:nif_error(not_loaded).

bind(_Socket, _Endpoint) ->
    erlang:nif_error(not_loaded).

connect(_Socket, _Endpoint) ->
    erlang:nif_error(not_loaded).

send(_Socket, _Binary, _Flags) ->
    erlang:nif_error(not_loaded).

recv(_Socket, _Flags) ->
    erlang:nif_error(not_loaded).

setsockopt(_Socket, _OptionName, _OptionValue) ->
    erlang:nif_error(not_loaded).

getsockopt(_Socket, _OptionName) ->
    erlang:nif_error(not_loaded).

close(_Socket) ->
    erlang:nif_error(not_loaded).

term(_Context) ->
    erlang:nif_error(not_loaded).

version() ->
    erlang:nif_error(not_loaded).
