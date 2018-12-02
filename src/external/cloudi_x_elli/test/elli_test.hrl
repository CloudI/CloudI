-define(I2B(I), list_to_binary(integer_to_list(I))).


status({ok, Status, _Headers, _ClientRef}) ->
    Status;
status({ok, Status, _Headers}) ->
    Status.


body({ok, _Status, _Headers, ClientRef}) ->
    {ok, Body} = hackney:body(ClientRef),
    Body.


headers({ok, _Status, Headers, _ClientRef}) ->
    lists:sort(Headers);
headers({ok, _Status, Headers}) ->
    lists:sort(Headers).
