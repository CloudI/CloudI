#`service` Behavior

A minimal Erlang behavior for creating CloudI internal services.

## LICENSE

[BSD License](https://github.com/CloudI/CloudI/blob/master/src/LICENSE)

## USING

Add the following to your internal service module after your exports:

    -include_lib("service/include/service.hrl").

Then implement the service behavior interface:

    -callback service_config() ->
        cloudi_service_api:service_internal() |
        cloudi_service_api:service_proplist().
    
    -callback service_init(Args :: list(),
                           Prefix :: cloudi_service:service_name_pattern(),
                           Dispatcher :: cloudi_service:dispatcher()) ->
        {'ok', State :: any()} |
        {'stop', Reason :: any()} |
        {'stop', Reason :: any(), State :: any()}.
    
    -callback service_request(ServiceReq :: service:service_req(),
                              State :: any(),
                              Dispatcher :: cloudi_service:dispatcher()) ->
        {'reply', Response :: cloudi_service:response(), NewState :: any()} |
        {'reply', ResponseInfo :: cloudi_service:response_info(),
         Response :: cloudi_service:response(), NewState :: any()} |
        {'forward', NextServiceReq :: service:service_req(),
         NewState :: any()} |
        {'noreply', NewState :: any()} |
        {'stop', Reason :: any(), NewState :: any()}.
    
    -callback service_info(Request :: any(),
                           State :: any(),
                           Dispatcher :: cloudi_service:dispatcher()) ->
        {'noreply', NewState :: any()} |
        {'stop', Reason :: any(), NewState :: any()}.
    
    -callback service_terminate(Reason :: any(),
                                State :: any()) ->
        'ok'.

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

## THANKS

* Juan Jose Comellas (interface ideas)

