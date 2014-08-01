%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Request==
%%% Request format transform.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_request).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/2,
         http_qs_parse/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% TODO: remove old code path?
new(Input, OutputType)
    when is_binary(Input) ->
    if
        OutputType =:= external ->
            Input;
        OutputType =:= internal ->
            cloudi_string:binary_to_term(Input)
    end;
new([I | _] = Input, internal)
    when is_integer(I) ->
    cloudi_string:list_to_term(Input);
new(Input, internal) ->
    Input.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse HTTP Request query string data.===
%% @end
%%-------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VER_16).
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

