%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Request==
%%% Request format transform.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([external_format/2]).

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
            {ok, Incoming} = cloudi_x_msgpack:
                             unpack(Request, [{map_format, map}]),
            Incoming
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

