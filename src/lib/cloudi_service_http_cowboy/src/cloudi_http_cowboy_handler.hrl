%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2014, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%%------------------------------------------------------------------------

%% cloudi_x_cowboy handler state
-record(cowboy_state,
    {
        dispatcher                :: cloudi_service:dispatcher(),
        context                   :: cloudi:context(),
        scope                     :: atom(),
        prefix                    :: string(),
        timeout_body              :: pos_integer(),
        timeout_part_header       :: pos_integer(),
        timeout_part_body         :: pos_integer(),
        timeout_websocket         :: infinity | pos_integer(),
        length_body_read          :: pos_integer(),
        length_body_chunk         :: pos_integer(),
        length_part_header_read   :: pos_integer(),
        length_part_header_chunk  :: pos_integer(),
        length_part_body_read     :: pos_integer(),
        length_part_body_chunk    :: pos_integer(),
        parts_destination_lock    :: boolean(),
        output_type               :: external | internal | list | binary,
        content_type_forced       :: undefined | binary(),
        content_types_accepted    :: undefined | binary:cp(),
        set_x_forwarded_for       :: boolean(),
        status_code_timeout       :: 100..599,
        websocket_output_type     :: text | binary,
        websocket_connect         :: undefined |
                                     {async | sync,
                                      cloudi_service:service_name()},
        websocket_disconnect      :: undefined |
                                     {async | sync,
                                      cloudi_service:service_name()},
        websocket_ping            :: undefined | received | pos_integer(),
        websocket_protocol        :: undefined |
                                     fun((incoming | outgoing, any()) ->
                                         {incoming | any(), any()}),
        websocket_name_unique     :: boolean(),
        websocket_subscriptions   :: undefined | cloudi_x_trie:cloudi_x_trie(),
        use_websockets            :: boolean() | exclusively,
        use_host_prefix           :: boolean(),
        use_client_ip_prefix      :: boolean(),
        use_method_suffix         :: boolean(),
        websocket_state           :: tuple()
    }).

