%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellstr�m <oscar@hellstrom.st>
-module(run_test).
-export([run/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(TEST_LOG, "test/error_logger.log").
-define(SASL_LOG, "test/sasl.log").
-define(FILE_NAME(MODULE),
    "cover_report/" ++ atom_to_list(MODULE) ++ ".html").

run() ->
    Modules = get_modules(),
    ok = cover_compile(Modules),
    start_logging(),
    Result = eunit:test(?MODULE, [verbose]),
    filelib:ensure_dir("cover_report/index.html"),
    html_report(Modules),
    write_report(Modules),
    stop_logging(),
    io:format("Cover report in cover_report/index.html~n"),
    io:format("Test logs in ~s and ~s~n", [?TEST_LOG, ?SASL_LOG]),
    if
        Result =:= ok -> halt(0);
        Result =/= ok -> halt(1)
    end.

start_logging() ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, {file, ?SASL_LOG}),
    file:delete(?TEST_LOG),
    file:delete(?SASL_LOG),
    error_logger:tty(false),
    error_logger:logfile({open, ?TEST_LOG}),
    application:start(sasl).

stop_logging() ->
    error_logger:logfile(close),
    application:stop(sasl).

html_report([Module | Modules]) ->
    cover:analyse_to_file(Module, ?FILE_NAME(Module), [html]),
    html_report(Modules);
html_report([]) ->
    ok.

write_report(Modules) ->
    {TotalPercentage, ModulesPersentage} = percentage(Modules, 0, 0, []),
    file:write_file("cover_report/index.html",
        [
            "<html>\n<head><title>Cover report index</title></head>\n"
            "<body>\n"
            "<h1>Cover report for lhttpc</h1>"
            "Total coverage: ", integer_to_list(TotalPercentage), "%"
            "<h2>Cover for individual modules</h2>\n"
            "<ul>\n\t",
            lists:foldl(fun({Module, Percentage}, Acc) ->
                        Name = atom_to_list(Module),
                        [
                            "<li>"
                            "<a href=\"", Name ++ ".html" "\">",
                            Name,
                            "</a> ", integer_to_list(Percentage), "%"
                            "</li>\n\t" |
                            Acc
                        ]
                end, [], ModulesPersentage),
            "</ul></body></html>"
        ]).

percentage([Module | Modules], TotCovered, TotLines, Percentages) ->
    {ok, Analasys} = cover:analyse(Module, coverage, line),
    {Covered, Lines} = lists:foldl(fun({_, {C, _}}, {Covered, Lines}) ->
                {C + Covered, Lines + 1}
        end, {0, 0}, Analasys),
    Percent = (Covered * 100) div Lines,
    NewPercentages = [{Module, Percent} | Percentages],
    percentage(Modules, Covered + TotCovered, Lines + TotLines, NewPercentages);
percentage([], Covered, Lines, Percentages) ->
    {(Covered * 100) div Lines, Percentages}.

get_modules() ->
    application:load(lhttpc),
    {ok, Modules} = application:get_key(lhttpc, modules),
    Modules.

cover_compile([Module | Modules]) ->
    {ok, Module} = cover:compile_beam(Module),
    cover_compile(Modules);
cover_compile([]) ->
    ok.

%%% Eunit functions
application_test_() ->
    {application, lhttpc}.
