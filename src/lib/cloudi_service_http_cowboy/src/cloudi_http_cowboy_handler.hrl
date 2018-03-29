%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%%------------------------------------------------------------------------

%% cloudi_x_cowboy handler state
-record(cowboy_state,
    {
        dispatcher :: cloudi_service:dispatcher(),
        timeout_async :: cloudi_service:timeout_value_milliseconds(),
        timeout_sync :: cloudi_service:timeout_value_milliseconds(),
        scope :: atom(),
        prefix :: string(),
        timeout_body :: pos_integer(),
        timeout_part_header :: pos_integer(),
        timeout_part_body :: pos_integer(),
        timeout_websocket :: infinity | pos_integer(),
        length_body_read :: pos_integer(),
        length_body_chunk :: pos_integer(),
        length_part_header_read :: pos_integer(),
        length_part_header_chunk :: pos_integer(),
        length_part_body_read :: pos_integer(),
        length_part_body_chunk :: pos_integer(),
        parts_destination_lock :: boolean(),
        output_type :: external | internal | list | binary,
        content_type_forced :: undefined | binary(),
        content_types_accepted :: undefined | binary:cp(),
        set_x_forwarded_for :: boolean(),
        status_code_timeout :: 100..599,
        query_get_format :: raw | text_pairs,
        websocket_output_type :: text | binary,
        websocket_connect
            :: undefined | {async | sync, cloudi_service:service_name()},
        websocket_disconnect
            :: undefined | {async | sync, cloudi_service:service_name()},
        websocket_ping :: undefined | received | pos_integer(),
        websocket_protocol
            :: undefined |
               fun((incoming | outgoing, any()) -> {incoming | any(), any()}),
        websocket_name_unique :: boolean(),
        websocket_subscriptions :: undefined | cloudi_x_trie:cloudi_x_trie(),
        use_websockets :: boolean() | exclusively,
        use_host_prefix :: boolean(),
        use_client_ip_prefix :: boolean(),
        use_x_method_override :: boolean(),
        use_method_suffix :: boolean(),
        websocket_state = undefined :: undefined | tuple()
    }).

