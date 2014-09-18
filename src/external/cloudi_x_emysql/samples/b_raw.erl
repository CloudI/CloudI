% ------------------------------------------------------------------------
% Hello World #2: A yet simpler sample usage of emysql
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 11 Jun 2010
% ------------------------------------------------------------------------
%
% This sample uses sasl, displays rawer output, and is more robust.
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
% $ ./b_raw
%
% - or - 
%
% Make emysql and start this sample directly, along these lines:
%
% $ cd Emysql
% $ make
% $ cd samples
% $ erlc b_raw.erl
% $ erl -pa ../ebin -s b_raw run -s init stop -noshell
%
% ------------------------------------------------------------------------
%
% Expected Output:
%
% ...
% ... many PROGRESS REPORT lines ...
% ...
%
% Result: {result_packet,5,
%                       [{field,2,<<"def">>,<<"hello_database">>,
%                               <<"hello_table">>,<<"hello_table">>,
%                               <<"hello_text">>,<<"hello_text">>,254,<<>>,33,
%                               60,0,0}],
%                       [[<<"Hello World!">>]],
%                       <<>>}
%
% ------------------------------------------------------------------------

-module(b_raw).
-export([run/0]).

run() ->

	%% -------------------------------------------------------------------
	%% Use sasl                                                          1
	%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
	application:start(sasl),
	
	%% -------------------------------------------------------------------
	
	crypto:start(),
	application:start(emysql),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	%% -------------------------------------------------------------------
	%% Get complete Result, not merely {_,_,_,Result,_}, as a_hello.erl  2
	%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    Result = emysql:execute(hello_pool,
    	<<"select hello_text from hello_table">>),
	
	%% ------------------------------------------------------------------- 

	io:format("~n~s~n", [string:chars($-,72)]),
	io:format("Result: ~p~n", [Result]),

    ok.
    
