%% vim: ts=4 sw=4 et
%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Mike Oxford <moxford@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% @deprecated Please use the functions in {@link emysql} instead.
-module(emysql_util).

%% Query data
-export([
	affected_rows/1,
	field_names/1,
	insert_id/1,
	result_type/1
]).

%% Conversion routines
-export([
         as_dict/1,
         as_json/1,
         as_proplist/1,
         as_record/3,
         as_record/4
]).

affected_rows(P) -> emysql:affected_rows(P).
field_names(R) -> emysql:field_names(R).
insert_id(P) -> emysql:insert_id(P).
result_type(R) -> emysql:result_type(R).

as_dict(Res) -> emysql:as_dict(Res).
as_json(Res) -> emysql:as_json(Res).
as_proplist(Res) -> emysql:as_proplist(Res).
as_record(Res, RecName, Fields) -> emysql:as_record(Res, RecName, Fields).
as_record(Res, RecName, Fields, Fun) -> emysql:as_record(Res, RecName, Fields, Fun).

