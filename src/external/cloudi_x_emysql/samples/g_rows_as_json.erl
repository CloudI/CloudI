% ------------------------------------------------------------------------
% Emysql: sample for accessing rows as json
% M. Dorner <dorner.michal@gmail.com>
% 17 Jun 2013
% ------------------------------------------------------------------------
%
% This sample does the same as the previous samples but converts row data to erlang representation of JSON.
% For actual JSON string use jsx (https://github.com/talentdeficit/jsx) or jiffy (https://github.com/davisp/jiffy).
%
% ------------------------------------------------------------------------
%
% Create local mysql database (or re-use the one made for a_hello):
%
% $ mysql ...
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
% mysql> quit
%
% On *nix build and run using the batch a_hello in folder samples/:
%
% $ ./g_rows_as_json
%
% - or -
%
% Make emysql and start this sample directly, along these lines:
%
% $ cd Emysql
% $ make
% $ cd samples
% $ erlc g_rows_as_json.erl
% $ erl -pa ../ebin -s g_rows_as_json run -s init stop -noshell
%
% ------------------------------------------------------------------------
%
% Expected Output:
%
% ...
% ... many PROGRESS REPORT lines ...
% ...
%
% JSON: [[{<<"hello_text">>,<<"Hello World!">>}]]
%
% ------------------------------------------------------------------------

-module(g_rows_as_json).
-export([run/0]).


run() ->

	application:start(sasl),
	crypto:start(),
	application:start(emysql),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(hello_pool, <<"SELECT * from hello_table">>),

	%% -------------------------------------------------------------------
	%% JSON Fetch:                                                      2
	%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	JSON = emysql:as_json(Result),

	%% -------------------------------------------------------------------

	io:format("~n~s~n", [string:chars($-,72)]),

	%% -------------------------------------------------------------------
	%% JSON Use   :                                                      3
	%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    io:format("JSON: ~p~n", [JSON]),

	%% -------------------------------------------------------------------

    ok.

