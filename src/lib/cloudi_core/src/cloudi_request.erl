%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Request==
%%% Request format transform.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([external_format/2,
         http_qs_parse/1]).

% erlang_binary:
%
% A raw format, making sure incoming binary data is preserved and
% outgoing non-binary data is made binary as with erlang_string
% (a human-readable format, same as file:consult/1).
% The erlang_binary format allows data to pass-through unmodified
% unless it requires modification for the external service to receive
% the service request response.  The erlang_binary format is only
% used by Erlang CloudI services that are able to handle raw Erlang
% binary data.
%
% erlang_string:
%
% Erlang terms as human-readable text for both incoming and outgoing
% data (same as file:consult/1).
%
% erlang_term:
%
% The Erlang Binary Term Format for both incoming and outgoing data
% (http://erlang.org/doc/apps/erts/erl_ext_dist.html).
%
% msgpack:
%
% The msgpack format with the following data modifications:
% outgoing Erlang atoms become msgpack binary strings (using utf8)
% outgoing Erlang tuples become msgpack arrays
% Erlang maps are used if the Erlang version >= 18.x
%   (otherwise the jsx map format is used for outgoing Erlang data)

% Usage:

% Typically an Elixir/Erlang CloudI service will provide an 'output'
% configuration argument (i.e., an "output type") which determines the
% Erlang types used for the service's outgoing CloudI service requests.
% The 'output' configuration argument value can then influence how the
% 'external_format' is used (where 'external_format' is also an
% Erlang CloudI service configuration argument).  The 'output'
% configuration argument values are below:

% external:
% The cloudi_request:external_format/2 function should always be called
% due to assuming the Request is an Erlang binary.
% The cloudi_response:external_format/2 function should always be called
% due to assuming the Response is an Erlang binary.

% internal:
% The cloudi_request:external_format/2 function should never be called
% on the Request data. The cloudi_response:external_format/2 function
% should never be called on the Response data.

% both:
% If the Request is an Erlang binary, the Request and Response are handled
% as if the 'output' is external.  If the Request is not an Erlang binary,
% the Request and Response are handled as if the 'output' is internal.

% The internal and external configuration argument values refer to whether
% the output is for either an internal CloudI service (Elixir/Erlang-only) or
% an external CloudI service (non-Elixir/Erlang).  External CloudI services
% use Erlang binary data for RequestInfo/Request data and
% ResponseInfo/Response data without any protocol enforced on the data
% (making the data protocol-agnostic).  The 'external_format' configuration
% argument specifies a protocol to use with the Request data and the Response
% data (RequestInfo and ResponseInfo are key/value metadata, which is a common
% concept in any protocol), based on the 'output' configuration argument.

% If the Erlang CloudI service needs an API with commands
% (normally wrapped into tuples, used for the Request data), then
% the 'output' configuration argument should support
% external, internal, and both. The 'external_format'
% configuration argument should also be supported.
%
% If the Erlang CloudI service's main purpose is to manage a source
% of Erlang binary data (a protocol, e.g., HTTP, TCP, UDP, etc.),
% then the 'output' configuration argument and the 'external_format'
% configuration argument should not be supported since the output should
% always be an Erlang binary (to keep the burden of binary validation on
% the CloudI service that receives the service requests).

-type external_format() ::
    erlang_string |
    erlang_term |
    msgpack.
-export_type([external_format/0]).

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Decode incoming external request data.===
%% @end
%%-------------------------------------------------------------------------

-spec external_format(Request :: binary(),
                      Format :: external_format()) ->
    any().

external_format(Request, Format)
    when is_binary(Request) ->
    if
        Format =:= erlang_string ->
            cloudi_string:binary_to_term(Request);
        Format =:= erlang_term ->
            erlang:binary_to_term(Request);
        Format =:= msgpack ->
            {ok, Incoming} = cloudi_x_msgpack:unpack(Request,
                                                     [{format, ?MSGPACK_MAP}]),
            Incoming
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse HTTP Request query string data.===
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_16).
-spec http_qs_parse(Request :: binary() |
                               list({any(), any()})) ->
    Result :: dict().
-else.
-spec http_qs_parse(Request :: binary() |
                               list({any(), any()})) ->
    Result :: dict:dict(binary(), binary()).
-endif.

http_qs_parse(Request) ->
    cloudi_request_info:key_value_parse(Request).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

