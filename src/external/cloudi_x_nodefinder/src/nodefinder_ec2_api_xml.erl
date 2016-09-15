%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% 
%%% Copyright (C) 2010 Brian Buchanan. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_xml.erl
%%%------------------------------------------------------------------------

-module(nodefinder_ec2_api_xml).
-export([decode/2, decode/3, get_bool/2, get_float/2, get_integer/2, get_list/2,
         get_text/1, get_text/2, get_text/3, get_time/2]).

-include_lib("xmerl/include/xmerl.hrl").

decode(Values, Node) ->
    lists:reverse(
      lists:foldl(
        fun ({Name, XPath, Type}, Output) ->
                case get_value(XPath, Type, Node) of
                    undefined -> Output;
                    Value ->
                        [{Name, Value}|Output]
                end
        end, [], Values)
     ).

decode(Values, Node, Record) ->
    lists:foldl(
      fun ({Index, XPath, Type}, Output) ->
              case get_value(XPath, Type, Node) of
                  undefined -> Output;
                  Value -> setelement(Index, Output, Value)
              end
      end, Record, Values
     ).

get_value(XPath, Type, Node) ->
    case Type of
        text -> get_text(XPath, Node);
        optional_text -> get_text(XPath, Node, undefined);
        integer -> get_integer(XPath, Node);
        optional_integer ->
            case get_text(XPath, Node, undefined) of
                undefined -> undefined;
                Text -> list_to_integer(Text)
            end;
        float -> get_float(XPath, Node);
        time -> get_time(XPath, Node);
        list -> get_list(XPath, Node);
        boolean -> get_bool(XPath, Node);
        optional_boolean ->
            case get_text(XPath, Node, undefined) of
                undefined -> undefined;
                "true" -> true;
                _ -> false
            end;
        present -> xmerl_xpath:string(XPath, Node) =/= [];
        xml -> Node;
        Fun when is_function(Fun, 1) ->
            Fun(xmerl_xpath:string(XPath, Node));
        {single, Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                [SubNode] -> Fun(SubNode)
            end;
        {map, Fun} when is_function(Fun, 1) ->
            lists:map(Fun, xmerl_xpath:string(XPath, Node));
        {optional_map, Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                List  -> lists:map(Fun, List)
            end;
        {single, List} when is_list(List) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> undefined;
                [SubNode] -> decode(List, SubNode)
            end;
        {value, Fun} when is_function(Fun, 1) ->
            Fun(get_text(XPath, Node));
        List when is_list(List) ->
            [decode(List, SubNode) || SubNode <- xmerl_xpath:string(XPath, Node)]
    end.

get_float(XPath, Node) ->
    list_to_float(get_text(XPath, Node)).

get_text(#xmlText{value=Value}) -> Value;
get_text(#xmlElement{content=Content}) ->
    lists:flatten([get_text(Node) || Node <- Content]).

get_text(XPath, Doc) -> get_text(XPath, Doc, "").
get_text({XPath, AttrName}, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/@" ++ AttrName, Doc) of
        [] -> Default;
        [#xmlAttribute{value=Value}|_] -> Value
    end;
get_text(XPath, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/text()", Doc) of
        [] -> Default;
        TextNodes ->
            lists:flatten([Node#xmlText.value || Node <- TextNodes])
    end.

get_list(XPath, Doc) ->
    [get_text(Node) || Node <- xmerl_xpath:string(XPath, Doc)].

get_integer(XPath, Doc) -> get_integer(XPath, Doc, 0).
get_integer(XPath, Doc, Default) ->
    case get_text(XPath, Doc) of
        "" -> Default;
        Text -> list_to_integer(Text)
    end.

get_bool(XPath, Doc) ->
    case get_text(XPath, Doc, "false") of
        "true" -> true;
        _ -> false
    end.

get_time(XPath, Doc) ->
    case get_text(XPath, Doc, undefined) of
        undefined -> undefined;
        Time -> parse_time(Time)
    end.

parse_time(String) ->
    case re:run(String, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(?:\\.\\d+)?Z", [{capture, all_but_first, list}]) of
        {match, [Yr, Mo, Da, H, M, S]} ->
            {{list_to_integer(Yr), list_to_integer(Mo), list_to_integer(Da)},
             {list_to_integer(H), list_to_integer(M), list_to_integer(S)}};
        nomatch ->
            error
    end.
